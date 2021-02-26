import BaseApi from './base';
import {StoreState} from '../store/type';
import {GcpToken} from '../types/gcp-state';


export default class GcpApi extends BaseApi<StoreState> {
  // Return value resolves to the token's expiry time if successful.
  refreshToken() {
    return this.spider('noun', 'gcp-token', 'get-gcp-token', {})
      .then((token) => {
        this.store.handleEvent({
          data: token
        });

        if (typeof(token) === 'object' &&
            typeof(token['gcp-token']) === 'object' &&
            typeof(token['gcp-token']['expiresIn']) === 'number') {
          return Promise.resolve(token['gcp-token']['expiresIn']);
        }
        return Promise.reject(new Error("invalid token"));
      });
  }
};
