export default class Subscription {
  constructor(store, api, channel) {
    this.store = store;
    this.api = api;
    this.channel = channel;
    this.channel.setOnChannelError(this.onChannelError.bind(this));
  }

  start() {
    if (this.api.ship) {
      this.initializeLinks();
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

  initializeLinks() {
    this.api.bind(
      '/all',
      'PUT',
      this.api.ship,
      'group-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );
    this.api.bind(
      '/primary',
      'PUT',
      this.api.ship,
      'contact-view',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );
    this.api.bind(
      '/primary',
      'PUT',
      this.api.ship,
      'invite-view',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );
    this.api.bind(
      '/app-name/link',
      'PUT',
      this.api.ship,
      'metadata-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );
    this.api.bind(
      '/app-name/contacts',
      'PUT',
      this.api.ship,
      'metadata-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );

    // open a subscription for what collections we're listening to
    this.api.bind(
      '/listening',
      'PUT',
      this.api.ship,
      'link-listen-hook',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );

    // open a subscription for all submissions
    this.api.getPage('', 0);

    // open a subscription for seen notifications
    this.api.bindLinkView(
      '/json/seen',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );
  }

  handleEvent(diff) {
    this.store.handleEvent(diff);
  }

  handleError(err) {
    console.error(err);
  }

  handleQuitSilently(quit) {
    // no-op
  }

  handleQuitAndResubscribe(quit) {
    // TODO: resubscribe
  }
}
