import React, { Component } from 'react';

const BCoin = window.BCoin;
const usb = navigator.usb;
import Ledger from '../../lib/ledger';


export default class ConnectLedger extends Component {

  constructor(props) {
    super(props);

    this.state = {
      connected: false,
      manager: new Ledger(props.network),
    }

    this.setLedgerAccount = this.setLedgerAccount.bind(this);
    this.deriveXPubKey = this.deriveXPubKey.bind(this);
  }

  async deriveXPubKey() {
    const { props, state } = this;
    console.log(props, state);
    if (state.connected || state.account === undefined)
      return;
    this.setState({ connected: true });
    const {
      pubkey,
      path,
      account
    } = await this.state.manager.deriveXPubKey(this.state.account);
    props.loadXPubKey(pubkey, path, account);
    this.setState({ connected: false });
  }

  setLedgerAccount() {
    this.setState({account: event.target.value});
  }

  render() {
    const { props, state, deriveXPubKey } = this;
    let createClasses = !state.connected
      ? "pointer db f9 mt2 green2 bg-gray0-d ba pv4 ph2 b--green2"
      : "pointer db f9 mt2 gray2 ba bg-gray0-d pv4 ph2 b--gray3";
    return (
      <div>
        <p className="f8 mt3 lh-copy db">Ledger</p>
        <div>
         <div className="cf w-30 fl pr2 overflow-x-hidden bg-gray0-d white-d flex flex-column">
            <button
              onClick={deriveXPubKey}
              className={createClasses}>
              { (state.connected) ? "Generating key": "Connect"}
            </button>
          </div>
          <div className="cf w-70 fl pr2 overflow-x-hidden bg-gray0-d white-d flex flex-column">
            <textarea
              className={
                "f9 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 " +
                "focus-b--black focus-b--white-d"
              }
              rows={1}
              placeholder="Account number to derive the extended pubkey"
              style={{
                resize: "none",
                height: 48,
                paddingTop: 10
              }}
              onChange={this.setLedgerAccount}
            />
          </div>
        </div>
      </div>
    );
  }
}
