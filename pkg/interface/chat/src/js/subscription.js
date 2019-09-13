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
      this.handleError.bind(this));
    api.bind('/all/updates', 'PUT', api.authTokens.ship, 'inbox',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    api.bind('/all', 'PUT', api.authTokens.ship, 'groups',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    api.bind('/all', 'PUT', api.authTokens.ship, 'permissions',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
  }

  handleEvent(diff) {
    store.handleEvent(diff);
  }

  handleError(err) {
    console.error(err);
  }
}

export let subscription = new Subscription();
