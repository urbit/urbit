import { api } from '/api';
import _ from 'lodash';
import { store } from '/store';


export class Subscription {
  start() {
    if (api.authTokens) {
      this.initializeChat();
      //this.setCleanupTasks();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  /*setCleanupTasks() {
    window.addEventListener("beforeunload", e => {
      api.bindPaths.forEach(p => {
        this.wipeSubscription(p);
      });
    });
  }

  wipeSubscription(path) {
    api.hall({
      wipe: {
        sub: [{
          hos: api.authTokens.ship,
          pax: path
        }]
      }
    });
  }*/

  initializeChat() {
    api.bind('/primary', 'PUT', api.authTokens.ship, 'chat',
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
