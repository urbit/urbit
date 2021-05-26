import { InitialReducer }  from './reducers/initial';
import { UpdateReducer }   from './reducers/update';
import { CurrencyReducer } from './reducers/currency';
import { SettingsReducer } from './reducers/settings';

class Store {
  constructor() {
    this.state = {
      loadedBtc: false,
      loadedSettings: false,
      loaded: false,
      providerPerms: {},
      shipWallets: {},
      provider: null,
      wallet: null,
      confirmedBalance: null,
      unconfirmedBalance: null,
      btcState: null,
      history: [],
      psbt: '',
      address: null,
      currencyRates: {
        BTC: { last: 1, symbol: 'BTC' }
      },
      denomination: 'BTC',
      showWarning: true,
      error: '',
      broadcastSuccess: false,
    };

    this.initialReducer = new InitialReducer();
    this.updateReducer = new UpdateReducer();
    this.currencyReducer = new CurrencyReducer();
    this.settingsReducer = new SettingsReducer();
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
    this.settingsReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
