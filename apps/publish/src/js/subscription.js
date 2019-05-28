import { api } from '/api';
import _ from 'lodash';
import { store } from '/store';


export class Subscription {
  start() {
    if (api.authTokens) {
      console.log("subscription.start", window.injectedState);
      this.initializeLandscape();
      this.setCleanupTasks();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  setCleanupTasks() {
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
  }


  initializeLandscape() {
    api.bind(`/primary`, "PUT", api.authTokens.ship, 'write',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
  }

  handleEvent(diff) {
    console.log("subscription.handleEvent", diff);
    store.handleEvent(diff);
  }

  handleError(err) {
    console.error(err);
    api.bind(`/primary`, "PUT", api.authTokens.ship, 'write',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
  }
}

export let subscription = new Subscription();
