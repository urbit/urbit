import { api } from './api';
import { store } from './store';

export class Subscription {
  start() {
    if (api.ship) {
      this.initializeBtcWallet();
      this.initializeSettings();
      this.initializeCurrencyPoll();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  initializeBtcWallet() {
    api.bind('/all', 'PUT', api.ship, 'btc-wallet',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
  }

  initializeSettings() {
    let app = 'settings-store';
    let path = '/bucket/btc-wallet';

    fetch(`/~/scry/${app}${path}.json`).then(res => res.json())
      .then(n => {
        this.handleEvent({data: n});
      });

    api.bind(path, 'PUT', api.ship, app,
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
    this.initializeSettings();
  }
}

export let subscription = new Subscription();
