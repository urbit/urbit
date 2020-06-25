import React, { Component } from 'react';
import { BrowserRouter, Route } from 'react-router-dom';

import { HeaderBar } from './components/lib/headerBar.js';
import ProgressBar from './components/lib/progressBar';
import BitcoinTransaction from './components/lib/bitcoinTransaction';
import ConnectLedger from './components/lib/connectLedger';

const BCoin = window.BCoin;

export class Root extends Component {
  constructor(props) {
    super(props);
    // this.state.seed = 'benefit crew supreme gesture quantum web media hazard theory mercy wing kitten';
    // this.state.progress = 0.0;
    this.state.peers = [];
    this.state.height = 0;
    this.state.hash = '';
    this.state.proxySocket = 'ws://127.0.0.1:9090';
    this.state.peerSeeds = ['127.0.0.1:48444'];
    this.state.network = 'regtest';
    this.state.account = 0;
    this.state.sent = false;

    this.loadMnemonic = this.loadMnemonic.bind(this);
    this.loadSocket = this.loadSocket.bind(this);
    this.startNode = this.startNode.bind(this);
    this.loadTrustedPeers = this.loadTrustedPeers.bind(this);
    this.selectNetwork = this.selectNetwork.bind(this);
    this.setPoint = this.setPoint.bind(this);
    this.setAmount = this.setAmount.bind(this);
    this.loadFromLedger = this.loadFromLedger.bind(this);
    this.keyFromSeed = this.keyFromSeed.bind(this);
  }

  componentDidMount() {
    document.title = 'OS1 - Wallet';
    this.props.subscription.startApp('bitcoin');
  }

  componentWillUnmount() {
    this.props.subscription.stopApp('bitcoin');
  }

  loadMnemonic(event) {
    this.setState({ seed: event.target.value });
  }

  loadFromLedger(xpubkey, path, account) {
    this.setState({ xpubkey: xpubkey, path: path, account: account });
  }

  loadTrustedPeers(event) {
    this.setState({ peerSeeds: [event.target.value] });
  }

  selectNetwork(event) {
    this.setState({ network: event.target.value });
  }

  loadSocket(event) {
    this.setState({ proxySocket: event.target.value });
  }

  setPoint(event) {
    this.setState({ point: event.target.value });
  }

  setAmount(event) {
    this.setState({ amount: BCoin.Amount.value(event.target.value) });
  }

  keyFromSeed(coinType, network) {
    const mnemonic = new BCoin.hd.Mnemonic(this.state.seed);
    const privKey = BCoin.hd.fromMnemonic(mnemonic);
    //  BIP 44: m / purpose' / coin_type' / account' / change / index
    //  Account 0 is sent to the %bitcoin app for address derivation
    const key = privKey.derivePath(`m/44'/${coinType}'/${this.state.account}'`);
    const xpub = key.xpubkey(network);
    this.setState({ xpubkey: xpub, keyring: key });
    return { xpub: xpub, master: key };
  }

  getInfo() {
    const { state } = this;
    const node = state.node;

    let addr = node.pool.hosts.getLocal();
    if (!addr) {
      addr = node.pool.hosts.address;
    };

    const peers = [];

    for (let peer = this.state.node.pool.peers.head(); peer; peer = peer.next) {
      peers.push({
        addr: peer.hostname(),
        subver: peer.agent,
        bytessent: peer.socket.bytesWritten,
        bytesrecv: peer.socket.bytesRead
      });
    }

    this.setState({
      progress: node.chain.getProgress(),
      height: node.chain.height,
      hash: node.chain.tip.rhash(),
      peers: peers
    });
  }

  async startNode() {
    const { api, network, proxySocket, seed, peerSeeds, xpubkey, hasXPub } = this.props;
    const config = {
      hash: true,
      network: network,
      memory: false,
      logConsole: true,
      workers: true,
      workerFile: '/~bitcoin/js/worker.js',
      createSocket: (port, host) => {
        return window.ProxySocket.connect(proxySocket, port, host);
      },
      logger: new window.Logger({
        level: 'info',
        console: true
      }),
      // https://github.com/bcoin-org/bcoin/issues/935
      // When running an SPV node with a wallet as a process there will be missing
      // transactions from blocks.
      // Solution: Run the SPV node and wallet in the same process as a plugin.
      plugins: [BCoin.wallet.plugin]
    };
    if (peerSeeds) {
      config.only = peerSeeds;
    }
    const spvNode = new BCoin.SPVNode(config);
    const { wdb } = spvNode.require('walletdb');

    await spvNode.ensure();
    await spvNode.open();
    await spvNode.connect();
    spvNode.startSync();
    const coinType = spvNode.network.keyPrefix.coinType;
    const networkType = spvNode.network.type;
    let xpub;
    let master;
    if (seed) {
      const keys = this.keyFromSeed(coinType, networkType);
      xpub = keys.xpub;
      master = keys.master;
    } else {
      xpub = BCoin.HDPublicKey.fromBase58(xpubkey).xpubkey(networkType);
    }
    const wallet = await wdb.ensure({
      id: window.ship,
      // master: master
      accountKey: xpub,
      watchOnly: true
    });

    // Debugging
    const ak = BCoin.HDPublicKey.fromBase58(xpub);
    const pk = ak.derive(0).derive(0);
    const addr2 = new BCoin.KeyRing(pk);
    const anAddr = BCoin.Address.fromBase58(
      addr2.getAddress('base58', spvNode.network.type)
    );
    console.log(
      addr2.getAddress('base58', spvNode.network.type),
      anAddr,
      'is this my address?',
      await wallet.hasAddress(anAddr),
      await wallet.getAccountByAddress(anAddr)
    );
    // Test

    if (!hasXPub || seed) {
      console.log('sending xpub');
      api.add.xpubkey(xpub);
    }

    wallet.on('balance', (balance) => {
      console.log('Balance updated:\n', balance.toJSON());
      this.setState({
        unConfirmedBalance: BCoin.Amount.btc(balance.confirmed),
        confirmedBalance: BCoin.Amount.btc(balance.unconfirmed)
      });
    });

    spvNode.on('block', async (block) => {
 this.getInfo();
});
    spvNode.on('block connect', async (block) => {
 this.getInfo();
});
    spvNode.on('tx', async (tx) => {
      await wdb.addTX(tx);
      const balance = await wallet.getBalance();
      this.setState({
        unConfirmedBalance: BCoin.Amount.btc(balance.confirmed),
        confirmedBalance: BCoin.Amount.btc(balance.unconfirmed)
      });
    });
    spvNode.pool.on('peer connect', () => {
      this.getInfo();
    });

    spvNode.pool.on('peer close', () => {
      this.getInfo();
    });

    spvNode.pool.on('peer open', () => {
      this.getInfo();
    });

    spvNode.pool.on('packet', () => {
      this.getInfo();
    });

    this.setState({
      wallet: wallet,
      node: spvNode,
      wdb: wdb,
      coinType: coinType
    });
  }

  render() {
    let { node, confirmedBalance, unConfirmedBalance, seed, xpubkey, height, hash, progress, network, peers, api, amount, point, address, wallet, wdb, keyring, account, coinType } = this.props;
    node = node ? node : {};
    const confirmed = confirmedBalance ? confirmedBalance : 0;
    const unconfirmed = unConfirmedBalance ? unConfirmedBalance : 0;

    const createClasses = (Boolean(seed) || xpubkey)
      ? 'pointer db f9 mt3 green2 bg-gray0-d ba pv3 ph4 b--green2'
      : 'pointer db f9 mt3 gray2 ba bg-gray0-d pa2 pv3 ph4 b--gray3';

    return (
      <BrowserRouter>
        <div className="absolute h-100 w-100 bg-gray0-d ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl">
          <HeaderBar />
          <Route exact path="/~bitcoin"
            render={ () => {
            return (
              <div className="cf w-100 flex flex-column pa4 ba-m ba-l ba-xl b--gray2 br1 h-100 h-100-minus-40-m h-100-minus-40-l h-100-minus-40-xl f9 white-d">
                <h1 className="mb3 f8">Bitcoin</h1>
                <div>
                  <div className={'cf w-20 fl pa2 overflow-x-hidden ' +
                                  'bg-gray0-d white-d flex flex-column'}
                  >
                    <button
                      onClick={this.startNode}
                      className={createClasses}
                    >
                      Start Node Sync
                    </button>
                  </div>
                  <div className={'cf w-60 fl pa2 pt4 overflow-x-hidden ' +
                                  'bg-gray0-d white-d flex flex-column'}
                  >
                    <div className="mono wrap">
                      Current Height: {height}
                    </div>
                    <div className="mono wrap">
                      Current Hash: {hash}
                    </div>
                    { ProgressBar(( (progress) ? progress : 0) )}
                  </div>
                  <div className="cf w-20 fl pa2 pt4 overflow-x-hidden bg-gray0-d white-d flex flex-column">
                    <div className="f6 mono wrap">
                      Balance: {confirmed}
                    </div>
                    <div className="f6 mono wrap">
                      Pending: {Math.abs(confirmed - unconfirmed)}
                    </div>
                  </div>
                </div>
                <div>
                  <div className={'cf w-50 fl pa2 pt4 overflow-x-hidden ' +
                                  'bg-gray0-d white-d flex flex-column'}
                  >
                    <div className="w-100">
                      <p className="f8 mt3 lh-copy db">Mnemonic seed</p>
                      <textarea
                        className={
                          'f9 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
                          'focus-b--black focus-b--white-d'
                        }
                        rows={1}
                        placeholder="benefit crew supreme gesture quantum web media hazard theory mercy wing kitten"
                        style={{
                          resize: 'none',
                          height: 48,
                          paddingTop: 14
                        }}
                        onChange={this.loadMnemonic}
                      />
                      <p className="f8 mt3 lh-copy db">Extended Public Key</p>
                          <div className="mono wrap">
                            <textarea
                              className={
                                'f9 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
                                'focus-b--black focus-b--white-d'
                              }
                              rows={1}
                              placeholder={xpubkey}
                              style={{
                                resize: 'none',
                                height: 48,
                                paddingTop: 14
                              }}
                            />
                          </div>
                      <ConnectLedger
                        loadXPubKey={this.loadFromLedger}
                        network={network}
                      />
                    </div>
                  </div>

                  <div className={'w-50 fl pa2 pt4 overflow-x-hidden ' +
                                  'bg-gray0-d white-d flex flex-column'}
                  >
                    <p className="f8 mt3 lh-copy db">Proxy Socket URL</p>
                    <textarea
                      className={
                        'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
                        'focus-b--black focus-b--white-d'
                      }
                      rows={1}
                      placeholder="ws://127.0.0.1:9090"
                      style={{
                        resize: 'none',
                        height: 48,
                        paddingTop: 14
                      }}
                      onChange={this.loadSocket}
                    />
                    <p className="f8 mt3 lh-copy db">Trusted Peer Nodes</p>
                    <textarea
                      className={
                        'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
                        'focus-b--black focus-b--white-d'
                      }
                      rows={1}
                      placeholder="No peers by default"
                      style={{
                        resize: 'none',
                        height: 48,
                        paddingTop: 14
                      }}
                      onChange={this.loadTrustedPeers}
                    />
                    <p className="f8 mt3 lh-copy db">Network</p>
                    <textarea
                      className={
                        'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
                        'focus-b--black focus-b--white-d'
                      }
                      rows={1}
                      placeholder="main, testnet or regtest"
                      style={{
                        resize: 'none',
                        height: 48,
                        paddingTop: 14
                      }}
                      onChange={this.selectNetwork}
                    />
                  </div>

                  <div className={'w-50 fl pa2 overflow-x-hidden ' +
                                  'bg-gray0-d white-d flex flex-column'}
                  >

                    <p className="f8 mt3 lh-copy db">Peer Nodes</p>
                    <div className={
                        'f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa1 db w-100 mt2 ' +
                        'focus-b--black focus-b--white-d'
                      }
                    >
                      <div className="dt dt--fixed f8 lh-copy db fw4">
                        <div className="fl w-third bb b--gray4 b--gray2-d gray2 tc">
                          Host
                        </div>
                        <div className="fl w-third bb b--gray4 b--gray2-d gray2 tc">
                          Agent
                        </div>
                        <div className="fl w-third bb b--gray4 b--gray2-d gray2 tc">
                          Bytes (↑↓)
                        </div>
                      </div>
                      {peers.map((peer, index) => {
                        const addr = peer.addr;
                        const subver = peer.subver;
                        const bytes = `${peer.bytessent}/${peer.bytesrecv}`;
                        return (
                          <div key={index} className="f9 dt dt--fixed">
                            <div className="fl w-third tc mono wrap">
                              {addr}
                            </div>
                            <div className="fl w-third tc mono wrap">
                              {subver}
                            </div>
                            <div className="fl w-third tc mono wrap">
                              {bytes}
                            </div>
                          </div>
                        );
                      })}
                    </div>
                  </div>

                  <div className={'w-50 fl pa2 overflow-x-hidden ' +
                                  'bg-gray0-d white-d flex flex-column'}
                  >
                      <p className="f8 mt3 lh-copy db">
                        Payments
                      </p>
                    <div className="w-100">
                      <div className="fl">
                        <BitcoinTransaction
                          amount={amount}
                          point={point}
                          api={api}
                          address={address}
                          network={network}
                          wallet={wallet}
                          node={node}
                          wdb={wdb}
                          keyring={keyring}
                          account={account}
                          coinType={coinType}
                        />
                      </div>
                      <div className="w-third fl pr2">
                        <textarea
                          className={
                            'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
                            'focus-b--black focus-b--white-d'
                          }
                          rows={1}
                          placeholder="~marzod"
                          style={{
                            resize: 'none',
                            height: 48,
                            paddingTop: 14
                          }}
                          onChange={this.setPoint}
                        />
                      </div>
                      <div className="w-third fl pr2">
                        <textarea
                          className={
                            'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
                            'focus-b--black focus-b--white-d'
                          }
                          rows={1}
                          placeholder="0.0 BTC"
                          style={{
                            resize: 'none',
                            height: 48,
                            paddingTop: 14
                          }}
                          onChange={this.setAmount}
                        />
                      </div>
                    </div>
                  </div>
                </div>
            </div>
            );
          }}
          />
        </div>
      </BrowserRouter>
    );
  }
}
