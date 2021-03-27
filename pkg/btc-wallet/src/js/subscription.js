import { api } from './api';
import { store } from './store';

export class Subscription {
  start() {
    if (api.authTokens) {
      this.initializeBtcWallet();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  initializeBtcWallet() {
    api.bind('/all', 'PUT', api.authTokens.ship, 'btc-wallet',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
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
