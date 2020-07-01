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
      walletDB: {},
      wallet: {}
    };
  }

  reduce(data, state) {
    this.initialReducer.reduce(data, state);
    this.addressReducer.reduce(data, state);
  }
}
