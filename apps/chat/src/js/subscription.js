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
    if (store.state.local) {
      let path = [];
      let msg = Object.keys(store.state.messages);
      for (let i = 0; i < msg.length; i++) {
        let cir = msg[i].split('/');
        if (cir.length > 1) {
          let hos = cir[0];
          if (urbitOb.isValidPatp(hos)) {
            let nom = cir[1];
            let len = 0;
            if (msg[i] in store.state.messages) {
              len = store.state.messages[msg[i]].length;
            }
            path.push(`${hos}/${nom}/${len}`);
          }
        } 
      }

      if (path.length <= 0) {
        path = '/primary';
      } else {
        path = '/primary/' + path.join('/');
      }
      console.log(path);

      api.bind(path, 'PUT', api.authTokens.ship, 'chat',
        this.handleEvent.bind(this),
        this.handleError.bind(this));
    } else {
      console.log('primary');
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
