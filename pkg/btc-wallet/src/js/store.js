import { InitialReducer }  from './reducers/initial';
import { UpdateReducer }   from './reducers/update';
import { CurrencyReducer } from './reducers/currency';

class Store {
  constructor() {
    this.state = {
      loaded: false,
      providerPerms: {},
      shipWallets: {},
      provider: null,
      wallet: null,
      balance: null,
      btcState: null,
      history: [],
      psbt: '',
      address: null,
      currencyRates: {
        BTC: { last: 1, symbol: '฿' }
      },
      denomination: 'BTC',
    };

    this.initialReducer = new InitialReducer();
    this.updateReducer = new UpdateReducer();
    this.currencyReducer = new CurrencyReducer();
    this.setState = () => { };
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    let json = data.data;
    this.initialReducer.reduce(json, this.state);
    this.updateReducer.reduce(json, this.state);
    this.currencyReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
