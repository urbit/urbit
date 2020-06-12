import { api } from '/api';
import { store } from '/store';

import urbitOb from 'urbit-ob';


export class Subscription {
  start() {
    if (api.authTokens) {
      this.initializebartinfo();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

   initializebartinfo() {
     api.bind("/routes", "PUT", api.authTokens.ship, "bartinfo",
       this.handleEvent.bind(this),
       this.handleError.bind(this));

     api.bind("/elevators", "PUT", api.authTokens.ship, "bartinfo",
       this.handleEvent.bind(this),
       this.handleError.bind(this));

     api.bind("/bartstations", "PUT", api.authTokens.ship, 'bartinfo',
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
