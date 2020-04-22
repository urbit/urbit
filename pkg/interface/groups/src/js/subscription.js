import { api } from '/api';
import { store } from '/store';

import urbitOb from 'urbit-ob';


export class Subscription {

  constructor() {
    this.firstRoundSubscriptionComplete = false;
  }

  start() {
    if (api.authTokens) {
      this.firstRoundSubscription();
      window.urb.setOnChannelError(this.onChannelError.bind(this));
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  onChannelError(err) {
    console.error('event source error: ', err);
    console.log('initiating new channel');
    this.firstRoundSubscriptionComplete = false;
    setTimeout(2000, () => {
      store.handleEvent({
        data: { clear : true}
      });
      this.start();
    });
  }

  subscribe(path, app) {
    api.bind(path, 'PUT', api.authTokens.ship, app,
      this.handleEvent.bind(this),
      (err) => {
        console.log(err);
        this.subscribe(path, app);
      },
      () => {
        this.subscribe(path, app);
      });
  }

  firstRoundSubscription() {
    this.subscribe('/primary', 'contact-view');
  }

  secondRoundSubscriptions() {
    this.subscribe('/synced', 'contact-hook');
    this.subscribe('/primary', 'invite-view');
    this.subscribe('/all', 'group-store');
    this.subscribe('/all', 'metadata-store');
  }

  handleEvent(diff) {
    if (!this.firstRoundSubscriptionComplete) {
      this.firstRoundSubscriptionComplete = true;
      this.secondRoundSubscriptions();
    }
    store.handleEvent(diff);
  }

}

export let subscription = new Subscription();
