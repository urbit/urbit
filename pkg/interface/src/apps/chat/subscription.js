export default class Subscription {
  constructor(store, api, channel) {
    this.store = store;
    this.api = api;
    this.channel = channel;

    this.channel.setOnChannelError(this.onChannelError.bind(this));
    this.firstRoundComplete = false;
  }

  start() {
    if (this.api.ship) {
      this.firstRound();
    } else {
      console.error('~~~ ERROR: Must set api.ship before operation ~~~');
    }
  }

  delete() {
    this.channel.delete();
  }

  onChannelError(err) {
    console.error('event source error: ', err);
    console.log('initiating new channel');
    this.firstRoundComplete = false;
    setTimeout(2000, () => {
      this.store.handleEvent({
        data: { clear : true }
      });

      this.start();
    });
  }

  subscribe(path, app) {
    this.api.bind(path, 'PUT', this.api.ship, app,
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
    this.subscribe('/primary', 'chat-view');
  }

  secondRoundSubscriptions() {
    this.subscribe('/synced', 'chat-hook');
    this.subscribe('/primary', 'invite-view');
    this.subscribe('/all', 'permission-store');
    this.subscribe('/primary', 'contact-view');
    this.subscribe('/app-name/chat', 'metadata-store');
    this.subscribe('/app-name/contacts', 'metadata-store');
  }

  handleEvent(diff) {
    if (!this.firstRoundComplete) {
      this.firstRoundComplete = true;
      this.secondRoundSubscriptions();
    }
    this.store.handleEvent(diff);
  }

  fetchMessages(start, end, path) {
    console.log(start, end, path);
    fetch(`/chat-view/paginate/${start}/${end}${path}`)
      .then(response => response.json())
      .then((json) => {
        this.store.handleEvent({
          data: json
        });
      });
  }
}

