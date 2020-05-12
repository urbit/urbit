export default class Subscription {
  constructor(store, api, channel) {
    this.store = store;
    this.api = api;
    this.channel = channel;

    this.channel.setOnChannelError(this.onChannelError.bind(this));
  }

  start() {
    if (this.api.ship) {
      this.initializePublish();
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
    setTimeout(2000, () => {
      this.store.handleEvent({
        data: { clear : true }
      });

      this.start();
    });
  }

  initializePublish() {
    this.api.bind('/primary', 'PUT', this.api.ship, 'publish',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    this.api.bind('/all', 'PUT', this.api.ship, 'group-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    this.api.bind('/primary', 'PUT', this.api.ship, 'contact-view',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    this.api.bind('/primary', 'PUT', this.api.ship, 'invite-view',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    this.api.bind('/all', 'PUT', this.api.ship, 'permission-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    this.api.bind('/app-name/contacts', 'PUT', this.api.ship, 'metadata-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
    }

  handleEvent(diff) {
    this.store.handleEvent(diff);
  }

  handleError(err) {
    console.error(err);
    this.api.bind('/primary', 'PUT', this.api.ship, 'publish',
      this.handleEvent.bind(this),
      this.handleError.bind(this));
  }
}
