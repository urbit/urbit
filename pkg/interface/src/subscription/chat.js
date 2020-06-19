import BaseSubscription from './base';

export default class ChatSubscription extends BaseSubscription {
  start() {
    this.subscribe('/primary', 'chat-view');
    setTimeout(() => {
      this.subscribe('/synced', 'chat-hook');
      this.subscribe('/all', 'invite-store');
      this.subscribe('/all', 'permission-store');
      this.subscribe('/primary', 'contact-view');
      this.subscribe('/app-name/chat', 'metadata-store');
      this.subscribe('/app-name/contacts', 'metadata-store');
    }, 1000);
  }


  fetchMessages(start, end, path) {
    fetch(`/chat-view/paginate/${start}/${end}${path}`)
      .then(response => response.json())
      .then((json) => {
        this.store.handleEvent({
          data: json
        });
      });
  }
}


