import { api } from '/api';
import { store } from '/store';

import urbitOb from 'urbit-ob';


export class Subscription {
  start() {
    if (api.authTokens) {
      this.initializeContacts();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  initializeContacts() {
    api.bind('/primary', 'PUT', api.authTokens.ship, 'invite-view',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this));
    api.bind('/all', 'PUT', api.authTokens.ship, 'group-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this));
      api.bind('/primary', 'PUT', api.authTokens.ship, 'contact-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this));
    api.bind('/all', 'PUT', api.authTokens.ship, 'permission-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this));
  }

  handleEvent(diff) {
    console.log(diff);
    store.handleEvent(diff);
  }

  handleError(err) {
    console.error(err);
  }

  handleQuitSilently(quit) {
    // no-op
  }

  handleQuitAndResubscribe(quit) {
    // TODO: resubscribe
  }
}

export let subscription = new Subscription();
