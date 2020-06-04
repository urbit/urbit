import BaseSubscription from './base';

export default class GlobalSubscription extends BaseSubscription {
  start() {
    this.subscribe('/all', 'invite-store');
    this.subscribe('/app-name/contacts', 'metadata-store');
  }
}

