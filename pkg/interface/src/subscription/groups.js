import BaseSubscription from './base';

export default class GroupsSubscription extends BaseSubscription {
  start() {
    this.subscribe('/primary', 'contact-view');
    this.subscribe('/all', 'group-store');
    this.subscribe('/all', 'metadata-store');
    this.subscribe('/synced', 'contact-hook');
    this.subscribe('/all', 'invite-store');
    this.subscribe('/all', 's3-store');
  }
}


