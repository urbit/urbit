import React, { Component } from 'react';
import ProgressBar from './lib/progressBar';
import BitcoinTransaction from './lib/bitcoinTransaction';

export class WalletList extends Component {
  render() {
    const { props } = this;

    const confirmed = props.confirmedBalance ? props.confirmedBalance : 0;
    const unconfirmed = props.unConfirmedBalance ? props.unConfirmedBalance : 0;

    const peers = (props.peers) ? props.peers : [];
    const node = props.node ? props.node : {};

    // const connectSPVClasses = (Boolean(state.seed) || state.xpubkey)
    //   ? 'pointer db f9 mt1 green2 bg-gray0-d ba pv1 ph1 b--green2'
    //   : 'pointer db f9 mt1 gray2 ba bg-gray0-d pa1 pv1 ph1 b--gray3';

    // const connectLocalClasses = (Boolean(state.walletPort) && Boolean(state.nodePort))
    //   ? 'pointer db f9 mt1 green2 bg-gray0-d ba pv1 ph1 b--green2'
    //   : 'pointer db f9 mt1 gray2 ba bg-gray0-d pa1 pv1 ph1 b--gray3';

    /* <div>
        <div className={'cf w-20 fl pa2 overflow-x-hidden ' +
          'bg-gray0-d white-d flex flex-column'}
        >
          <button
                      onClick={this.startNode}
                      className={connectSPVClasses}
                    >
                      Sync Browser SPV Node
                    </button>
                    <button
                      onClick={this.connectClientNode}
                      className={connectLocalClasses}
                    >
                      Connect to Bcoin Node
                    </button>
    </div>
    <div className={'cf w-60 fl pa2 pt4 overflow-x-hidden ' +
          'bg-gray0-d white-d flex flex-column'}
        >
          <div className="mono wrap">
            Current Height: {(props.height) ? props.height : 0}
          </div>
          <div className="mono wrap">
            Current Hash: {(props.hash) ? props.hash : ''}
          </div>
          {ProgressBar(((props.progress) ? props.progress : 0))}
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
          </div>
        </div>

        <div className={'w-50 fl pa2 pt4 overflow-x-hidden ' +
          'bg-gray0-d white-d flex flex-column'}
        >
          <div>
            <div className={'w-third fl overflow-x-hidden ' +
              'bg-gray0-d white-d flex flex-column'}
            >
              <p className="f8 mt3 lh-copy db">Proxy Socket URL</p>
              <textarea
                className={
                  'f9 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
                  'focus-b--black focus-b--white-d'
                }
                rows={1}
                placeholder="ws://127.0.0.1:9090"
                style={{
                  resize: 'none',
                  height: 48,
                  paddingTop: 14
                }}
                onChange={props.loadSocket}
              />
            </div>
            <div className={'w-third fl overflow-x-hidden ' +
              'bg-gray0-d white-d flex flex-column'}
            >
              <p className="f8 mt3 lh-copy db">Local BCoin Node</p>
              <textarea
                className={
                  'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
                  'focus-b--black focus-b--white-d'
                }
                rows={1}
                placeholder="48334"
                style={{
                  resize: 'none',
                  height: 48,
                  paddingTop: 14
                }}
                onChange={props.loadNodePort}
              />
            </div>
            <div className={'w-third fl overflow-x-hidden ' +
              'bg-gray0-d white-d flex flex-column'}
            >
              <p className="f8 mt3 lh-copy db">Local BCoin Wallet</p>
              <textarea
                className={
                  'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
                  'focus-b--black focus-b--white-d'
                }
                rows={1}
                placeholder="48335"
                style={{
                  resize: 'none',
                  height: 48,
                  paddingTop: 14
                }}
                onChange={props.loadWalletPort}
              />
            </div>
          </div>

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
            onChange={props.loadTrustedPeers}
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
            onChange={props.selectNetwork}
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
          <div>
            <div className="w-70 fl pr2">
              <p className="f8 mt3 lh-copy db">
                Payments
                        </p>
              <div className="w-100">
                <div className="fl">
                  <BitcoinTransaction
                    amount={props.amount}
                    point={props.point}
                    api={props.api.wallet}
                    address={props.address}
                    network={props.network}
                    wallet={props.wallet}
                    node={node}
                    wdb={props.wdb}
                    keyring={props.keyring}
                    account={props.account}
                    coinType={props.coinType}
                    connectedTo={props.connectedTo}
                    signMethod={props.signMethod}
                  />
                </div>
                <div className="w-40 fl pr2">
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
                    onChange={props.setPoint}
                  />
                </div>
                <div className="w-30 fl pr2">
                  <textarea
                    className={
                      'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
                      'focus-b--black focus-b--white-d'
                    }
                    rows={1}
                    placeholder="0 BTC"
                    style={{
                      resize: 'none',
                      height: 48,
                      paddingTop: 14
                    }}
                    onChange={props.setAmount}
                  />
                </div>
              </div>
            </div>
            {(props.connectedTo === 'spvNode') ?
              <div className="w-30 fl pr2">
                <p className="f8 mt3 lh-copy db">Signing</p>
                <div className="w-100 fl pr2 pa3">
                  <input type="radio" id="seed"
                    name="sign"
                    value="seed" onChange={props.handleSigning}
                  />
                  <label className="pl2 f8" htmlFor="seed">Seed</label><br />
                  <input defaultChecked type="radio"
                    id="ledger"
                    name="sign" value="ledger"
                    onChange={props.handleSigning}
                  />
                  <label className="pl2 f8" htmlFor="ledger">Ledger</label><br />
                </div>
              </div>
              : null
            }
          </div>
        </div>
      </div> */

    return (
      <div className="w-100 flex justify-center">
      <div className="w-100 mw6">
      </div>
      </div>
    );
  }
}

export default WalletList;
