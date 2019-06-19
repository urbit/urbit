import { api } from '/api';
import _ from 'lodash';
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
    api.bind('/primary', 'PUT', api.authTokens.ship, 'chat',
      this.handleEvent.bind(this),
      this.handleError.bind(this));

    /*window.addEventListener('beforeunload', (event) => {
      // Cancel the event as stated by the standard.
      event.preventDefault();
      // Chrome requires returnValue to be set.
      event.returnValue = '';

      if (window.subscriptionId) {
        window.urb.unsubscribe(window.subscriptionId);
      }
    });*/
  }

  fetchMessages(circle, start, end) {
    fetch(`/~chat/scroll/${circle}/${start}/${end}`)
      .then((response) => response.json())
      .then((json) => {
        console.log('handled', json);
        store.handleEvent({
          data: json
        });
      });
  }

  handleEvent(diff) {
    store.handleEvent(diff);
  }

  handleError(err) {
    console.error(err);
    api.bind('/', "PUT", api.authTokens.ship, 'chat',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
  }
}

export let subscription = new Subscription();
