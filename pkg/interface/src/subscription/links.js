import BaseSubscription from './base';

export default class LinksSubscription extends BaseSubscription {
  start() {
    this.subscribe('/all', 'group-store');
    this.subscribe('/primary', 'contact-view');
    this.subscribe('/all', 'invite-store');
    this.subscribe('/app-name/link', 'metadata-store');
    this.subscribe('/app-name/contacts', 'metadata-store');
    this.subscribe('/listening', 'link-listen-hook');

    // open a subscription for all submissions
    this.api.getPage('', 0);

    this.subscribe('/json/seen', 'link-view');
  }
}

