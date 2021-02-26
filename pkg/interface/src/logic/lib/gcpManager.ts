// Singleton that manages GCP token state.
//
// To use:
//
// 1. call configure with a GlobalApi and GlobalStore.
// 2. call start() to start the token refresh loop.
//
// If the ship has S3 credentials set, we don't try to get a token, but we keep
// checking at regular intervals to see if they get unset. Otherwise, we try to
// invoke the GCP token thread on the ship until it gives us an access token.
// Once we have a token, we refresh it every hour or so, since it has an
// intrinsic expiry.
//
//
import GlobalApi from '../api/global';
import GlobalStore from '../store/store';


class GcpManager {
  #api: GlobalApi | null = null;
  #store: GlobalStore | null = null;

  configure(api: GlobalApi, store: GlobalStore) {
    this.#api = api;
    this.#store = store;
  }

  #running = false;
  #timeoutId: number | null = null;

  start() {
    if (this.#running) {
      console.warn('GcpManager already running');
      return;
    }
    if (!this.#api || !this.#store) {
      console.error('GcpManager must have api and store set');
      return;
    }
    this.#running = true;
    this.refreshLoop();
  }

  stop() {
    if (!this.#running) {
      console.warn('GcpManager already stopped');
      console.assert(this.#timeoutId === null);
      return;
    }
    this.#running = false;
    if (this.#timeoutId !== null) {
      clearTimeout(this.#timeoutId);
      this.#timeoutId = null;
    }
  }

  restart() {
    if (this.#running)
      this.stop();
    this.start();
  }

  #consecutiveFailures: number = 0;

  private refreshLoop() {
    const s3 = this.#store.state.s3;
    // XX ships currently always have S3 credentials, but the fields are all
    // set to '' if they are not configured.
    if (s3 &&
        s3.credentials &&
        s3.credentials.accessKeyId &&
        s3.credentials.secretAccessKey) {
      // do nothing, and check again in 5s.
      this.refreshAfter(5_000);
      return;
    }
    this.#api.gcp.refreshToken()
      .then(() => {
        const token = this.#store.state.gcp?.token;
        if (token) {
          this.#consecutiveFailures = 0;
          const interval = this.refreshInterval(token.expiresIn);
          console.log('GcpManager got token; refreshing after', interval);
          this.refreshAfter(interval);
        } else {
          throw new Error('thread succeeded, but returned no token?');
        }
      })
      .catch((reason) => {
        this.#consecutiveFailures++;
        console.warn('GcpManager refresh failed; retrying with backoff');
        this.refreshAfter(this.backoffInterval());
      });
  }

  private refreshAfter(durationMs) {
    if (!this.#running)
      return;
    if (this.#timeoutId !== null) {
      console.warn('GcpManager already has a timeout set');
      return;
    }
    this.#timeoutId = setTimeout(() => {
      this.#timeoutId = null;
      this.refreshLoop();
    }, durationMs);
  }

  private refreshInterval(expiresIn: number) {
    // Give ourselves a minute for processing delays, but never refresh sooner
    // than 30 minutes from now. (The expiry window should be about an hour.)
    return Math.max(30 * 60_000, expiresIn - 60_000);
  }

  private backoffInterval() {
    // exponential backoff.
    const slotMs = 5_000;
    const maxSlot = 60;   // 5 minutes
    const backoffSlots =
      Math.floor(Math.random() * Math.min(maxSlot, this.#consecutiveFailures));
    return slotMs * backoffSlots;
  }
}

const instance = new GcpManager();
Object.freeze(instance);

export default instance;
