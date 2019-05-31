import { api } from '/api';
import _ from 'lodash';
import { store } from '/store';


export class Subscription {
  start() {
    if (api.authTokens) {
      this.initializeChat();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  initializeChat() {
    if (store.state.local) {
      let path = [];
      let msg = Object.keys(store.state.messages);
      for (let i = 0; i < msg.length; i++) {
        let cir = msg[i];
        let len = store.state.messages[cir].length;
        path.push(`${cir}/${len}`);
      }
      path = path.join('/');

      api.bind(`/primary/${path}`, 'PUT', api.authTokens.ship, 'chat',
        this.handleEvent.bind(this),
        this.handleError.bind(this));
    } else {
      api.bind('/primary', 'PUT', api.authTokens.ship, 'chat',
        this.handleEvent.bind(this),
        this.handleError.bind(this));
    }

    api.bind('/updates', 'PUT', api.authTokens.ship, 'chat',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
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
