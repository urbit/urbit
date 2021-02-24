import BaseApi from './base';
import {StoreState} from '../store/type';
import {GcpToken} from '../types/gcp-state';


export default class GcpApi extends BaseApi<StoreState> {
  #running = false;

  startRefreshLoop() {
    if (this.#running) {
      console.error('GcpApi startRefreshLoop: already running');
    } else {
      this.#running = true;
      this.refreshLoop();
    }
  }

  private refreshLoop() {
    console.log("GcpApi refreshLoop");
    this.refreshToken().then(({accessKey, expiresIn}: GcpToken) => {
      console.log("GcpApi new token");
      // XX maybe bad?
      this.store.state.gcp.accessKey = accessKey;
      const timeout = this.refreshInterval(expiresIn);
      console.log("GcpApi refreshing in", timeout);
      setTimeout(() => {
        this.refreshLoop();
      }, timeout);
    });
  }

  private refreshInterval(expiresIn: number) {
    // Give ourselves 30 seconds for processing delays, but never refresh
    // sooner than 10 minute from now. (The expiry window should be about an
    // hour.)
    return Math.max(600_000, expiresIn - 30_000);
  }

  private async refreshToken() {
    const token = await this.spider('noun', 'gcp-token', 'get-gcp-token', {});
    return token['gcp-token'];
  }
};
