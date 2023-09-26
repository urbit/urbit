import { api } from '/api';
import { store } from '/store';

export class Subscription {
  start() {
    if (api.authTokens) {
      //
    } else {
      console.error("~~~ ERROR: Must set api.authTokens before operation ~~~");
    }
  }

  handleEvent(diff) {
    store.handleEvent(diff);
  }

  handleError(err) {
    console.error(err);
  }

  handleQuitSilently(quit) {
    // no-op
  }

  handleQuitAndResubscribe(quit) {
    // TODO: resubscribe
  }

}

export let subscription = new Subscription();
