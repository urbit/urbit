import _ from 'lodash';
import { api } from '/lib/api';
import { store } from '/store';


export class Subscription {

  constructor() {
    this.bindPaths = [];
    this.authTokens = null;
  }

  setAuthTokens(authTokens) {
    this.authTokens = authTokens;
  }

  subscribe(path, ship = this.authTokens.ship) {
    let bindPaths = _.uniq([...this.bindPaths, path]);
    if (bindPaths.length == this.bindPaths.length) {
      return;
    }

    this.bindPaths = bindPaths;

    api.bind("home", path,
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      ship);
  }

  handleEvent(diff) {
    store.handleEvent(diff);
  }

  handleError(err, app, path, ship) {
    api.bind(app, path,
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      ship);
  }
}

export let subscription = new Subscription();
window.subscription = subscription;
