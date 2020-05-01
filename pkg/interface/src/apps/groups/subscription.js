import api from './api';
import { store } from './store';

export class Subscription {
  constructor() {
    this.firstRoundComplete = false;
    this.secondRoundComplete = false;
  }

  start(channel) {
    if (api.authTokens) {
      this.firstRound();
     channel.setOnChannelError(this.onChannelError.bind(this));
    } else {
      console.error('~~~ ERROR: Must set api.authTokens before operation ~~~');
    }
  }

  onChannelError(err) {
    console.error('event source error: ', err);
    console.log('initiating new channel');
    this.firstRoundComplete = false;
    this.secondRoundComplete = false;
    setTimeout(2000, () => {
      store.handleEvent({
        data: { clear : true }
      });
      this.start();
    });
  }

  subscribe(path, app) {
    api.bind(path, 'PUT', api.authTokens.ship, app,
      this.handleEvent.bind(this),
      (err) => {
        console.log(err);
        this.subscribe(path, app);
      },
      () => {
        this.subscribe(path, app);
      });
  }

  firstRound() {
    this.subscribe('/primary', 'contact-view');
  }

  secondRound() {
    this.subscribe('/all', 'group-store');
    this.subscribe('/all', 'metadata-store');
  }

  thirdRound() {
    this.subscribe('/synced', 'contact-hook');
    this.subscribe('/primary', 'invite-view');
    this.subscribe('/all', 's3-store');
  }

  handleEvent(diff) {
    if (!this.firstRoundComplete) {
      this.firstRoundComplete = true;
      this.secondRound();
    } else if (!this.secondRoundComplete) {
      this.secondRoundComplete = true;
      this.thirdRound();
    }
    store.handleEvent(diff);
  }
}

const subscription = new Subscription();
export default subscription;
