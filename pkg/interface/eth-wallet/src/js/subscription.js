import { api } from '/api';
import { store } from '/store';

import urbitOb from 'urbit-ob';


export class Subscription {
  start() {
    if (api.authTokens) {
      this.initializeetheventviewer();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  initializeetheventviewer() {
    api.bind('/primary', 'PUT', api.authTokens.ship, 'eth-event-viewer',
      this.handleEvent.bind(this),
      this.handleError.bind(this));

    api.bind(
      "/state/update",
      "PUT",
      api.authTokens.ship,
      "eth-event-viewer",
      this.handleEvent.bind(this),
      this.handleError.bind(this)
    );
  }

  handleEvent(diff) {
    store.handleEvent(diff);
  }

  handleError(err) {
    console.error(err);
    api.bind('/primary', 'PUT', api.authTokens.ship, 'eth-event-viewer',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
  }
}

export let subscription = new Subscription();
