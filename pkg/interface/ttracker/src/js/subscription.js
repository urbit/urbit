import { api } from '/api';
import { store } from '/store';

import urbitOb from 'urbit-ob';


export class Subscription {
  start() {
    if (api.authTokens) {
      this.getTTrackerInfo();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  getTTrackerInfo() {
    api.bind("/mbtaalert", "PUT", api.authTokens.ship, "ttracker",
       this.handleEvent.bind(this),
       this.handleError.bind(this));

     api.bind("/mbtastation", "PUT", api.authTokens.ship, "ttracker",
       this.handleEvent.bind(this),
       this.handleError.bind(this));

      api.bind("/mbtaroutes", "PUT", api.authTokens.ship, "ttracker",
       this.handleEvent.bind(this),
       this.handleError.bind(this));

       api.bind("/mbtafacility", "PUT", api.authTokens.ship, "ttracker",
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