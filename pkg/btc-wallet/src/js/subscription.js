import { api } from './api';
import { store } from './store';

export class Subscription {
  start() {
    if (api.ship) {
      this.initializeBtcWallet();
      this.initializeCurrencyPoll();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  initializeBtcWallet() {
    console.log('initialize');
    api.bind('/all', 'PUT', api.ship, 'btc-wallet',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
  }

  initializeCurrencyPoll() {
    fetch('https://blockchain.info/ticker')
      .then(res => res.json())
      .then(n => {
        store.handleEvent({data: {currencyRates: n}})
        setTimeout(this.initializeCurrencyPoll, 1000 * 60 * 15);
      });
  }

  handleEvent(diff) {
    store.handleEvent(diff);
  }

  handleError(err) {
    console.error(err);
    this.initializeBtcWallet();
  }
}

export let subscription = new Subscription();
