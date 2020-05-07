import api from './api';
import store from './store';

export class Subscription {
  start(channel) {
    if (api.authTokens) {
      this.initializeLinks();
      channel.setOnChannelError(this.onChannelError.bind(this));
    } else {
      console.error('~~~ ERROR: Must set api.authTokens before operation ~~~');
    }
  }

  onChannelError(err) {
    console.error('event source error: ', err);
    console.log('initiating new channel');
    setTimeout(2000, () => {
      store.handleEvent({
        data: { clear : true }
      });
      this.start();
    });
  }

  initializeLinks() {
    api.bind(
      '/all',
      'PUT',
      api.authTokens.ship,
      'group-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );
    api.bind(
      '/primary',
      'PUT',
      api.authTokens.ship,
      'contact-view',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );
    api.bind(
      '/primary',
      'PUT',
      api.authTokens.ship,
      'invite-view',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );
    api.bind(
      '/app-name/link',
      'PUT',
      api.authTokens.ship,
      'metadata-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );
    api.bind(
      '/app-name/contacts',
      'PUT',
      api.authTokens.ship,
      'metadata-store',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );

    // open a subscription for what collections we're listening to
    api.bind(
      '/listening',
      'PUT',
      api.authTokens.ship,
      'link-listen-hook',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );

    // open a subscription for all submissions
    api.getPage('', 0);

    // open a subscription for seen notifications
    api.bindLinkView(
      '/json/seen',
      this.handleEvent.bind(this),
      this.handleError.bind(this),
      this.handleQuitAndResubscribe.bind(this)
    );
  }

  handleEvent(diff) {
    store.handleEvent(diff);
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

const subscription = new Subscription();

export default subscription;
