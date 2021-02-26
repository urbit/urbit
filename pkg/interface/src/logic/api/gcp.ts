import BaseApi from './base';
import {StoreState} from '../store/type';
import {GcpToken} from '../types/gcp-state';


export default class GcpApi extends BaseApi<StoreState> {
  refreshToken() {
    return this.spider('noun', 'gcp-token', 'get-gcp-token', {})
      .then((token) => {
        this.store.handleEvent({
          data: token
        });
      });
  }
};
