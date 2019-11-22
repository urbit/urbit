import { api } from './api';
import { store } from './store';


export class Subscription {
  start() {
    if (api.authTokens) {
      this.initializesoto();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  initializesoto() {
    api.bind('/sole/'+api.authTokens.dojoId,
      'PUT', api.authTokens.ship, 'dojo',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
  }

  handleEvent(diff) {
    store.handleEvent(diff);
}

  handleError(err) {
    console.error(err);
    api.bind('/sole/'+api.authTokens.dojoId,
      'PUT', api.authTokens.ship, 'dojo',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
  }
}

export let subscription = new Subscription();
