import _ from 'lodash';

export class InitialReducer {
  reduce(json, state) {
    let data = _.get(json, 'initial', false);
    if (data) {
      state.provider = data.provider;
      state.wallet = data.wallet;
      state.confirmedBalance = _.get(data.balance, 'confirmed', null);
      state.unconfirmedBalance = _.get(data.balance, 'unconfirmed', null);
      state.btcState = data['btc-state'];
      state.history = this.reduceHistory(data.history);
      state.address = data.address;

      state.loadedBtc = true;
      if (state.loadedSettings) {
        state.loaded = true;
      }
    }
  }

  reduceHistory(history) {
    return Object.values(history).sort((hest1, hest2) => {
      if (hest1.recvd === null) return -1;
      if (hest2.recvd === null) return +1;
      return (hest2.recvd - hest1.recvd)
    })
  }
}
