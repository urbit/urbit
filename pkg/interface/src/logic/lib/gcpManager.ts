// Singleton that manages GCP token state.
//
// To use:
//
// 1. call configure with a GlobalApi and GlobalStore.
// 2. call start() to start the token refresh loop.
//
// If the ship does not have GCP storage configured, we don't try to get
// a token, but we keep checking at regular intervals to see if it gets
// configured. If GCP storage is configured, we try to invoke the GCP
// get-token thread on the ship until it gives us an access token.  Once
// we have a token, we refresh it every hour or so according to its
// intrinsic expiry.
//
//
import GlobalApi from '../api/global';
import useStorageState from '../state/storage';


class GcpManager {
  #api: GlobalApi | null = null;

  configure(api: GlobalApi) {
    this.#api = api;
  }

  #running = false;
  #timeoutId: number | null = null;

  start() {
    if (this.#running) {
      console.warn('GcpManager already running');
      return;
    }
    if (!this.#api) {
      console.error('GcpManager must have api set');
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
    if (this.#running) {
      this.stop();
    }
    this.start();
  }

  #consecutiveFailures: number = 0;

  private isConfigured() {
    return useStorageState.getState().gcp.configured;
  }

  private refreshLoop() {
    if (!this.isConfigured()) {
      this.#api.gcp.isConfigured()
        .then(() => {
          if (this.isConfigured() === undefined) {
            throw new Error("can't check whether GCP is configured?");
          }
          if (this.isConfigured()) {
            this.refreshLoop();
          } else {
            this.refreshAfter(10_000);
          }
        })
        .catch((reason) => {
          console.error('GcpManager failure; stopping.', reason);
          this.stop();
        });
      return;
    }
    this.#api.gcp.getToken()
      .then(() => {
        const token = useStorageState.getState().gcp.token;
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
        console.warn('GcpManager token refresh failed; retrying with backoff');
        this.refreshAfter(this.backoffInterval());
      });
  }

  private refreshAfter(durationMs) {
    if (!this.#running) {
      return;
    }
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
