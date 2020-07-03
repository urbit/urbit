import React from 'react';
import { BrowserRouter, Route } from 'react-router-dom';

import AddWallet from './components/addwallet';
import WalletList from './components/walletList';

import BCoin from '../../lib/bcoin';
import ProxySocket from '../../lib/proxy';
import Logger from '../../lib/logger';

export default class WalletApp extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      proxySocket: 'ws://127.0.0.1:9090',
      peerSeeds: ['127.0.0.1:48444'],
      network: 'regtest',
      account: 0,
      sent: false,
      hasXPub: false
    };

    this.loadMnemonic = this.loadMnemonic.bind(this);
    this.loadSocket = this.loadSocket.bind(this);
    this.startNode = this.startNode.bind(this);
    this.connectClientNode = this.connectClientNode.bind(this);
    this.loadTrustedPeers = this.loadTrustedPeers.bind(this);
    this.selectNetwork = this.selectNetwork.bind(this);
    this.setPoint = this.setPoint.bind(this);
    this.setAmount = this.setAmount.bind(this);
    this.loadFromLedger = this.loadFromLedger.bind(this);
    this.keyFromSeed = this.keyFromSeed.bind(this);
    this.loadNodePort = this.loadNodePort.bind(this);
    this.loadWalletPort = this.loadWalletPort.bind(this);
    this.handleSigning = this.handleSigning.bind(this);
  }

  componentDidMount() {
    document.title = 'OS1 - Wallet';

    this.props.subscription.startApp('wallet');
  }

  componentDidUpdate() {
    if (this.props.xpubkey && !this.state.xpubkey) {
      this.setState({ xpubkey: this.props.xpubkey, hasXPub: true }, () => {
        // sync transactions if we know the key
        this.startNode();
      });
    }
    if (this.props.address && !this.state.address) {
      this.setState({ address: this.props.address });
    }
  }

  handleSigning(event) {
    this.setState({ signMethod: event.target.value });
  }

  loadMnemonic(event) {
    this.setState({ seed: event.target.value });
  }

  loadNodePort(event) {
    this.setState({ nodePort: event.target.value });
  }

  loadWalletPort(event) {
    this.setState({ walletPort: event.target.value });
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
    console.log('setPoint', event.target.value),
    this.setState({ point: event.target.value });
  }

  setAmount(event) {
    console.log('setAmount', event.target.value),
    this.setState({ amount: BCoin.Amount.value(event.target.value) });
  }

  keyFromSeed(coinType, network) {
    console.log(coinType, network);
    const mnemonic = new BCoin.hd.Mnemonic(this.state.seed);
    const privKey = BCoin.hd.fromMnemonic(mnemonic);
    //  BIP 44: m / purpose' / coin_type' / account' / change / index
    const key = privKey.derivePath(`m/44'/${coinType}'`);
    const accountKey = key.derive(this.state.account, true);
    const xpub = accountKey.xpubkey(network);
    this.setState({ xpubkey: xpub, keyring: accountKey });
    return { xpub: xpub, master: key };
  }

  getInfo() {
    const { state } = this;
    const peers = [];
    for (let peer = state.node.pool.peers.head(); peer; peer = peer.next) {
      peers.push({
        addr: peer.hostname(),
        subver: peer.agent,
        bytessent: peer.socket.bytesWritten,
        bytesrecv: peer.socket.bytesRead
      });
    }
    this.setState({
      progress: state.node.chain.getProgress(),
      height: state.node.chain.height,
      hash: state.node.chain.tip.rhash(),
      peers: peers
    });
  }

  async startNode() {
    const { state, props } = this;
    if (!(Boolean(state.seed) || Boolean(state.xpubkey)) || state.connected)
      return;
    const config = {
      hash: true,
      network: state.network,
      memory: false,
      logConsole: true,
      workers: true,
      workerFile: '/~landscape/js/worker.js',
      createSocket: (port, host) => {
        return ProxySocket.connect(this.state.proxySocket, port, host);
      },
      logger: new Logger({
        level: 'info',
        console: true
      }),
      // https://github.com/bcoin-org/bcoin/issues/935
      // When running an SPV node with a wallet as a process there will be missing
      // transactions from blocks.
      // Solution: Run the SPV node and wallet in the same process as a plugin.
      plugins: [BCoin.wallet.plugin]
    };
    if (state.peerSeeds) {
      config.only = state.peerSeeds;
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
    if (state.seed) {
      const keys = this.keyFromSeed(coinType, networkType);
      xpub = keys.xpub;
      master = keys.master;
    } else {
      xpub = BCoin.HDPublicKey.fromBase58(state.xpubkey).xpubkey(networkType);
    }
    const wallet = await wdb.ensure({
      id: ship,
      // master: master
      accountKey: xpub,
      watchOnly: true
    });

    if ((state.xpubkey !== '' && !state.hasXPub) || state.seed) {
      props.api.wallet.addXPubKey(xpub);
    }

    wallet.on('balance', (balance) => {
      console.log('Balance updated:\n', balance.toJSON());
      const nTX = balance.tx;
      const coins = balance.coin;
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
      const nTX = balance.tx;
      const coins = balance.coin;
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

    const balance = await wallet.getBalance();

    this.setState({
      wallet: wallet,
      node: spvNode,
      wdb: wdb,
      coinType: coinType,
      spvNode: true,
      connectedTo: 'spvNode',
      connected: true,
      unConfirmedBalance: BCoin.Amount.btc(balance.confirmed),
      confirmedBalance: BCoin.Amount.btc(balance.unconfirmed),
      progress: spvNode.chain.getProgress(),
      height: spvNode.chain.height,
      hash: spvNode.chain.tip.rhash()
    });
  }

  parseChainEntry(raw) {
    const chain = BCoin.ChainEntry.fromRaw(raw);
    const start = 1231006505;
    const current = chain.time - start;
    const end = Math.floor(Date.now() / 1000) - start - 40 * 60;
    return {
      progress: (chain.height === 0) ? 0 : Math.min(1, current / end),
      height: chain.height,
      hash: Buffer.from(chain.hash).reverse().toString('hex')
    };
  }

  async connectClientNode() {
    const { state, props } = this;
    if (!(Boolean(state.nodePort) && Boolean(state.walletPort)) || state.connected)
      return;
    const network = BCoin.Network.get(this.state.network);
    const walletClient = new BCoin.WalletClient({
      port: 48334, // this.state.walletPort
      network: network.type
    });
    const nodeClient = new BCoin.NodeClient({
      port: 48445, // this.state.nodePort
      network: network.type
    });

    walletClient.bind('balance', (walletID, balance) => {
      console.log('New Balance:');
      console.log(walletID, balance);
      this.setState({
        unConfirmedBalance: BCoin.Amount.btc(balance.confirmed),
        confirmedBalance: BCoin.Amount.btc(balance.unconfirmed)
      });
    });
    walletClient.bind('address', (walletID, receive) => {
      console.log('New Receiving Address:');
      console.log(walletID, receive);
    });
    walletClient.bind('tx', (walletID, details) => {
      console.log('New Wallet TX:');
      console.log(walletID, details);
    });

    // listen for new blocks
    nodeClient.bind('block connect', (raw, txs) => {
      console.log('Node - Block Connect Event:\n', BCoin.ChainEntry.fromRaw(raw));
      const chain = BCoin.ChainEntry.fromRaw(raw);
      const { progress, height, hash } = this.parseChainEntry(raw);
      this.setState({
        progress: progress,
        height: height,
        hash: hash
      });
    });

    nodeClient.on('connect', async (e) => {
      try {
        console.log('Node - Connect event:\n', e);
        // `watch chain` subscribes us to chain events like `block`
        console.log('Node - Attempting watch chain:\n', await nodeClient.call('watch chain'));

        // `watch mempool` Subscribe to mempool/pool events:
        console.log('Node - Attempting watch mempool:\n', await nodeClient.call('watch mempool'));
        // Some calls simply request information from the server like an http request
        console.log('Node - Attempting get tip:');
        const tip = await nodeClient.call('get tip');
        const { progress, height, hash } = this.parseChainEntry(tip);
        const peers = await nodeClient.execute('getpeerinfo');
        this.setState({
          progress: progress,
          height: height,
          hash: hash,
          peers: peers
        });
      } catch (e) {
        console.log('Node - Connection Error:\n', e);
      }
    });

    // Authenticate and join wallet after connection to listen for events
    walletClient.on('connect', async () => {
      // Join - All wallets
      const wallets = await walletClient.getWallets();
      console.log('wallets', wallets);
      let wallet;
      if (wallets.includes(ship)) {
        wallet = walletClient.wallet(ship);
        await walletClient.call('join', ship);
      } else if (this.state.seed) {
        const keys = this.keyFromSeed(
          network.keyPrefix.coinType,
          network.type
        );
        const xpub = keys.xpub;
        const master = keys.master;
        console.log(xpub, master.isMaster(), master.toJSON(network.type));
        try {
          await walletClient.createWallet(ship, {
            mnemonic: this.state.seed
            // master: master.toBase58(network.type)
            // watchOnly: true,
            // accountKey: xpub
          });
        } catch (e) {
          console.log('Wallet: ', e);
        }
        wallet = walletClient.wallet(ship);
        await walletClient.call('join', ship);
        console.log('sending xpub');
        props.api.wallet.addXPubKey(xpub);
      } else {
        // if (!this.state.hasXPub ) {
          console.log(`ERROR: a wallet [${ship}] does not exist`);
        // } else {
        //   wallet = walletClient.wallet(ship);
        //   console.log(wallet);
        //   await walletClient.call('join', ship);
        // }
      }
      if (wallet) {
        const balance = await wallet.getBalance();
        console.log(balance);
        this.setState({
          wallet: wallet,
          unConfirmedBalance: BCoin.Amount.btc(balance.confirmed),
          confirmedBalance: BCoin.Amount.btc(balance.unconfirmed)
        });
      }
    });

    // open socket to listen for events
    await walletClient.open();
    await nodeClient.open();

    console.log(nodeClient);
    this.setState({
      connectedTo: 'localNode',
      connected: true
    });
  }

  render() {
    const { state, props } = this;

    return (
      <BrowserRouter>
        <div className="absolute h-100 w-100 bg-gray0-d ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl" style={{ height: 'calc(100% - 45px)' }}>
          <div className="cf w-100 pa4 ba-m ba-l ba-xl b--gray2 br1 h-100 h-100-minus-40-m h-100-minus-40-l h-100-minus-40-xl f9 white-d">
          <Route exact path="/~wallet"
            render={ () => {
            return (
              <WalletList
                height={state.height}
                hash={state.hash}
                progress={state.progress}
                confirmedBalance={state.confirmedBalance}
                unConfirmedBalance={state.unConfirmedBalance}
                loadSocket={this.loadSocket}
                loadNodePort={this.loadNodePort}
                loadWalletPort={this.loadWalletPort}
                loadTrustedPeers={this.loadTrustedPeers}
                selectNetwork={this.selectNetwork}
                peers={state.peers || []}
                amount={state.amount}
                point={state.point}
                api={props.api}
                address={state.address}
                network={state.network}
                wallet={state.wallet}
                node={state.node || {}}
                wdb={state.wdb}
                keyring={state.keyring}
                account={state.account}
                coinType={state.coinType}
                connectedTo={state.connectedTo}
                signMethod={state.signMethod}
                setPoint={this.setPoint}
                setAmount={this.setAmount}
                handleSigning={this.handleSigning}
              />
            );
          }}
          />
          <Route exact path="/~wallet/add"
            render={ () => {
              return (
                <AddWallet
                  loadMnemonic={this.loadMnemonic}
                  xpubkey={state.xpubkey}
                  loadFromLedger={this.loadFromLedger}
                  network={state.network}
                />
              );
            }}
          />
        </div>
        </div>
      </BrowserRouter>
    );
  }
}
