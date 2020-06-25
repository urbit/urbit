import React, { Component } from 'react';
import urbitOb from "urbit-ob";

import Ledger from '../../lib/ledger';

const BCoin = window.BCoin;
const Path = window.BPath.Path;

const STATE = {
  INIT: 'INIT',
  POINT: 'POINT',
  READY: 'READY',
  COMPLETED: 'COMPLETED'
};


export default class BitcoinTransaction extends Component {

  constructor(props) {
    super(props);
    this.state = {
      manager: new Ledger(props.network)
    }
    this.buttonMessage = this.buttonMessage.bind(this);
    this.requestAddress = this.requestAddress.bind(this);
    this.sendBTCTransaction = this.sendBTCTransaction.bind(this);
  }

  buttonMessage(status, amount, point) {
    const label =
        status === STATE.INIT      ? `Send`
      : status === STATE.POINT     ? `Request address`
      : status === STATE.READY     ? 'Send'
      // : status === STATE.READY     ? `Send ${BCoin.Amount.btc(amount)}BTC to ${point}`
      : status === STATE.COMPLETED ? 'Transaction Completed'
      : 'error!';
    return label
  }

  requestAddress () {
      this.setState({
        error: false,
        awaitingAddres: true
      }, () => {
        api.request.address(
          this.props.point.replace("~", ""),
          this.props.network
        ).then(() => {
          this.setState({awaitingAddres: false, status: STATE.READY});
        })
      });
  }

  async sendBTCTransaction () {
    const { state, props } = this;
    const {
      wallet, address, node, wdb, amount, keyring, coinType, account
    } = props;
    const type = wallet.network.keyPrefix.coinType;
    let mtx = await wallet.createTX({
      outputs: [{
        value: BCoin.Amount.value(amount),
        address: address.toString(node.network),
      }],
    });
    const inputs = await wallet.getInputPaths(mtx);
    if (keyring) {
      const rings = [];
      for (const input of inputs) {
        const ring = new BCoin.wallet.WalletKey(
          keyring.derive(input.branch).derive(input.index).privateKey);
        if (ring)
          rings.push(ring);
      }
      // Signing
      mtx.sign(rings);
    } else {
      const inputData = [];
      // From: https://github.com/bcoin-org/bsigner/blob/e8c9b80ee9559b3b32f16ff5c3d9f5d23b7b921c/test/utils/common.js#L83
      for (const input of mtx.inputs) {
          const data = {};
          const prevhash = input.prevout.hash;
          const txRecord = await wallet.getTX(prevhash);
          const coin = mtx.view.getCoinFor(input);
          const path = Path.fromList([44, coinType, account], true);
          const base = path.clone();
          const address = coin.getAddress();
          const addressPath = await wallet.getPath(address.getHash());
          data.coin = coin;
          data.prevTX = BCoin.TX.fromOptions(txRecord.tx);
          data.path = base.push(addressPath.branch).push(addressPath.index);
          // This will fail with Nested addresses.
          data.witness = input.type === BCoin.Address.types.WITNESS;
          inputData.push(data);
      }
      // Signing
      mtx = await this.state.manager.signTransaction(mtx, inputData);
    }
    // The transaction should now verify.
    if (mtx.verify()) {
      let tx = mtx.toTX();
      if (!BCoin.TX.isTX(tx)) {
        tx = BCoin.TX.fromOptions({
          version: tx.version,
          inputs: tx.inputs,
          outputs: tx.outputs,
          locktime: tx.locktime
        });
      }
      const ans = await node.sendTX(tx);
      await wdb.addTX(tx);
    } else {
      console.log("ERROR tx won't verify...", mtx, keyring);
    }
  }

  render() {
    const { props, state, buttonMessage, requestAddress, sendBTCTransaction } = this;
    const isDefaultState = ( props.point === undefined || props.amount === undefined );
    let isValidPoint;
    if (props.point) {
      isValidPoint = (!isDefaultState && urbitOb.isValidPatp(props.point));
    }
    const isReady = (isValidPoint && props.address !== undefined);

    let createClasses = !isDefaultState
      ? "pointer db f9 mt2 mr2 green2 bg-gray0-d ba pv4 ph4 b--green2"
      : "pointer db f9 mt2 mr2 gray2 ba bg-gray0-d pa2 pv4 ph4 b--gray3";

    return (
      <>
        { isDefaultState ? (
            <button
              className={createClasses}>
              { buttonMessage(STATE.INIT, props.amount, props.point) }
            </button>
        ): null}

        { (isValidPoint && !isReady) ? (
            <button
              onClick={requestAddress}
              className={createClasses}>
              { buttonMessage(STATE.POINT, props.amount, props.point) }
            </button>
        ) : null}

        { isReady ? (
            <button
              onClick={sendBTCTransaction}
              className={createClasses}>
              { buttonMessage(STATE.READY, props.amount, props.point) }
            </button>
        ): null}
      </>

    );
  }
}
