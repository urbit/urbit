import BaseStore from './base';
import WalletInitialReducer from '../reducers/bitcoin-initial';
import WalletAddressReducer from '../reducers/bitcoin-address';


export default class WalletStore extends BaseStore {
  constructor() {
    super();
    this.initialReducer = new WalletInitialReducer();
    this.addressReducer = new WalletAddressReducer();
  }

  initialState() {
    return {
      proxySocket: 'ws://127.0.0.1:9090',
      peerSeeds:['127.0.0.1:48444'],
      network: 'regtest',
      account: 0,
      sent:false
    };
  }

  reduce(data, state) {
    this.initialReducer.reduce(data, state);
    this.addressReducer.reduce(data, state);
  }
}
