export default class BaseSubscription {
  constructor(store, api, channel) {
    this.store = store;
    this.api = api;
    this.channel = channel;
    this.channel.setOnChannelError(this.onChannelError.bind(this));
  }

  delete() {
    this.channel.delete();
  }

  onChannelError(err) {
    console.error('event source error: ', err);
    setTimeout(2000, () => {
      this.store.clear();
      this.start();
    });
  }

  subscribe(path, app) {
    this.api.subscribe(path, 'PUT', this.api.ship, app,
      this.handleEvent.bind(this),
      (err) => {
        console.log(err);
        this.subscribe(path, app);
      },
      () => {
        this.subscribe(path, app);
      });
  }

  start() {
    // extend
  }

  handleEvent(diff) {
    // extend
    this.store.handleEvent(diff);
  }
}

