import _ from 'lodash';


export class UpdateReducer {
  reduce(json, state) {
    if (!json) {
      return;
    }
    if (json.providerStatus) {
      this.reduceProviderStatus(json.providerStatus, state);
    }
    if (json.checkPayee) {
      this.reduceCheckPayee(json.checkPayee, state);
    }
    if ("change-provider" in json) {
      this.reduceChangeProvider(json["change-provider"], state);
    }
    if (json["change-wallet"]) {
      this.changeWallet(json["change-wallet"], state);
    }
    if (json.hasOwnProperty('psbt')) {
      this.reducePsbt(json.psbt, state);
    }
    if (json["btc-state"]) {
      this.reduceBtcState(json["btc-state"], state);
    }
    if (json["new-tx"]) {
      this.reduceNewTx(json["new-tx"], state);
    }
    if (json["cancel-tx"]) {
      this.reduceCancelTx(json["cancel-tx"], state);
    }
    if (json.address) {
      this.reduceAddress(json.address, state);
    }
    if (json.balance) {
      this.reduceBalance(json.balance, state);
    }
    if (json.hasOwnProperty('error')) {
      this.reduceError(json.error, state);
    }
  }

  reduceProviderStatus(json, state) {
    state.providerPerms[json.provider] = json.permitted;
  }

  reduceCheckPayee(json, state) {
    state.shipWallets[json.payee] = json.hasWallet;
  }

  reduceChangeProvider(json, state) {
    state.provider = json;
  }

  reduceChangeWallet(json, state) {
    state.wallet = json;
  }

  reducePsbt(json, state) {
    state.psbt = json;
  }

  reduceBtcState(json, state) {
    state.btcState = json;
  }

  reduceNewTx(json, state) {
    let old = _.findIndex(state.history, (h) => {
      return ( h.txid.dat === json.txid.dat &&
               h.txid.wid === json.txid.wid );
    });
    if (old !== -1) {
      delete state.history.splice(old, 1);
    }
    if (json.recvd === null) {
      state.history.unshift(json);
    } else {
      // we expect history to have null recvd values first, and the rest in
      // descending order
      let insertionIndex = _.findIndex(state.history, (h) => {
        return ((h.recvd < json.recvd) && (h.recvd !== null));
      });
      state.history.splice(insertionIndex, 0, json);
    }
  }

  reduceCancelTx(json, state) {
    let entryIndex = _.findIndex(state.history, (h) => {
      return ((json.wid === h.txid.wid) && (json.dat === h.txid.dat));
    });
    if (entryIndex > -1) {
      state.history[entryIndex].failure = true;
    }
  }

  reduceAddress(json, state) {
    state.address = json;
  }

  reduceBalance(json, state) {
    state.unconfirmedBalance = json.unconfirmed;
    state.confirmedBalance = json.confirmed;
  }

  reduceError(json, state) {
    state.error = json;
  }
}
