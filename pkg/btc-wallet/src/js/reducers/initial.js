import _ from 'lodash';

export class InitialReducer {
  reduce(json, state) {
    let data = _.get(json, 'initial', false);
    if (data) {
      console.log('InitialReducer', data);
      state.provider = data.provider;
      state.wallet = data.wallet;
      state.balance = data.balance;
      state.btcState = data['btc-state'];
      state.history = this.reduceHistory(data.history);
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
