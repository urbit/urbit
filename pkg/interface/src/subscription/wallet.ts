import BaseSubscription from './base';

export default class WalletSubscription extends BaseSubscription {
  start() {
    this.subscribe('/primary', 'wallet');
  }
}
