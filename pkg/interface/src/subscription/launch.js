import BaseSubscription from './base';

export default class LaunchSubscription extends BaseSubscription {
  start() {
    this.subscribe('/all', 'launch');
    this.subscribe('/all', 'weather');
  }
}

