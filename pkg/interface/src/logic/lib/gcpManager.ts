// Singleton that manages GCP token state.
//
// To use:
//
// 1. call setApi with a GlobalApi.
// 2. call start() to start the token refresh loop.
//
//
import GlobalApi from '../api/global';


class GcpManager {
  #api: GlobalApi | null = null;

  setApi(api: GlobalApi) {
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
    if (this.#running)
      this.stop();
    this.start();
  }

  private refreshLoop() {
    this.#api.gcp.refreshToken()
      .then(
        (expiresIn: number) => {
          this.refreshAfter(this.refreshInterval(expiresIn));
        })
      .catch(
        (reason) => {
          console.error('GcpManager token refresh failed', reason);
          this.refreshAfter(30_000);  // XX backoff?
        });
  }

  private refreshAfter(durationMs) {
    if (!this.#running)
      return;
    if (this.#timeoutId !== null) {
      console.warn('GcpManager already has a timeout set');
      return;
    }
    console.log('GcpManager refreshing after', durationMs, 'ms');
    this.#timeoutId = setTimeout(() => {
      this.#timeoutId = null;
      this.refreshLoop();
    }, durationMs);
  }

  private refreshInterval(expiresIn: number) {
    // Give ourselves 30 seconds for processing delays, but never refresh
    // sooner than 30 minutes from now. (The expiry window should be about an
    // hour.)
    return Math.max(30 * 60_000, expiresIn - 30_000);
  }
}

const instance = new GcpManager();
Object.freeze(instance);

export default instance;
