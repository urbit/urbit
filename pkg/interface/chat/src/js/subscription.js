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
    this.subscribe('/primary', 'chat-view');
  }

  secondRoundSubscriptions() {
    this.subscribe('/synced', 'chat-hook');
    this.subscribe('/primary', 'invite-view');
    this.subscribe('/all', 'permission-store');
    this.subscribe('/primary', 'contact-view');
    this.subscribe('/app-name/chat', 'metadata-store');
    this.subscribe('/app-name/contacts', 'metadata-store');
  }

  handleEvent(diff) {
    if (!this.firstRoundSubscriptionComplete) {
      this.firstRoundSubscriptionComplete = true;
      this.secondRoundSubscriptions();
    }
    store.handleEvent(diff);
  }

  fetchMessages(start, end, path) {
    console.log(start, end, path);
    fetch(`/~chat/paginate/${start}/${end}${path}`)
      .then((response) => response.json())
      .then((json) => {
        store.handleEvent({
          data: json
        });
      });
  }

}

export let subscription = new Subscription();
