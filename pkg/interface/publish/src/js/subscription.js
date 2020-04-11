import { api } from '/api';
import { store } from '/store';


export class Subscription {
  start() {
    if (api.authTokens) {
      this.initializePublish();
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  initializePublish() {
    api.bind(`/primary`, "PUT", api.authTokens.ship, 'publish',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    api.bind('/all', 'PUT', api.authTokens.ship, 'group-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    api.bind('/primary', 'PUT', api.authTokens.ship, 'contact-view',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    api.bind('/primary', 'PUT', api.authTokens.ship, 'invite-view',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    api.bind('/all', 'PUT', api.authTokens.ship, 'permission-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    api.bind('/app-name/contacts', 'PUT', api.authTokens.ship, 'metadata-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    }

  handleEvent(diff) {
    store.handleEvent(diff);
  }

  handleError(err) {
    console.error(err);
    api.bind(`/primary`, "PUT", api.authTokens.ship, 'publish',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
  }
}

export let subscription = new Subscription();
