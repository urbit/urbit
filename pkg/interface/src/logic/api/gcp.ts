import BaseApi from './base';
import {StoreState} from '../store/type';
import {GcpToken} from '../types/gcp-state';


export default class GcpApi extends BaseApi<StoreState> {
  startRefreshLoop() {
    // TODO: don't allow more than one
    this.refreshLoop();
  }

  private refreshLoop() {
    console.log("refreshing GCP token");
    this.refreshToken().then(({accessKey, expiresIn}: GcpToken) => {
      console.log("new token: ", accessKey);
      setTimeout(() => {
        this.refreshLoop();
      }, expiresIn);
    });
  }

  private async refreshToken() {
    const token = await this.spider('noun', 'gcp-token', 'get-gcp-token', {});
    return token['gcp-token'];
  }
};
