import { api } from '/api';
import { store } from '/store';

import urbitOb from 'urbit-ob';


export class Subscription {
  start() {
    if (api.authTokens) {
      this.initializeChat();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  initializeChat() {
    api.bind('/inbox', 'PUT', api.authTokens.ship, 'inbox-view',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitSilently.bind(this));
    api.bind('/updates', 'PUT', api.authTokens.ship, 'inbox-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this));
    api.bind('/all', 'PUT', api.authTokens.ship, 'group-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this));
    api.bind('/all', 'PUT', api.authTokens.ship, 'permission-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this));
  }

  handleEvent(diff) {
    store.handleEvent(diff);
  }

  handleError(err) {
    console.error(err);
  }

  handleQuitSilently(quit) {
    // no-op
  }

  handleQuitAndResubscribe(quit) {
    console.error(quit);
    // TODO: resubscribe
  }

}

export let subscription = new Subscription();
