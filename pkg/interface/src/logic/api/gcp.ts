import BaseApi from './base';
import {StoreState} from '../store/type';


export default class GcpApi extends BaseApi<StoreState> {
  startRefreshLoop() {
    this.refreshToken();
  }

  private refreshToken() {
    const res = this.spider('noun', 'gcp-token', 'get-gcp-token', {});
    // TODO: store the token, set a timer to refresh it
  }
};
