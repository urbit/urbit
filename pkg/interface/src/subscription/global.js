import BaseSubscription from './base';

export default class GlobalSubscription extends BaseSubscription {
  start() {
    this.subscribe('/primary', 'invite-view');
    this.subscribe('/app-name/contacts', 'metadata-store');
  }
}

