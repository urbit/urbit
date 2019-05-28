import _ from 'lodash';
import { api } from '/lib/api';


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

    api.bind("chess", path,
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      ship);
  }

  handleEvent(diff) {
    console.log(diff);
  }

  handleError(err, app, path, ship) {
    api.bind(app, path,
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      ship);
  }
}

export let subscription = new Subscription();
