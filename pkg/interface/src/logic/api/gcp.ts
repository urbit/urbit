import BaseApi from './base';
import {StoreState} from '../store/type';
import {GcpToken} from '../types/gcp-state';


export default class GcpApi extends BaseApi<StoreState> {
  isConfigured() {
    return this.spider('noun', 'json', 'gcp-is-configured', {})
      .then((data) => {
        this.store.handleEvent({
          data
        });
      });
  }

  getToken() {
    return this.spider('noun', 'gcp-token', 'gcp-get-token', {})
      .then((token) => {
        this.store.handleEvent({
          data: token
        });
      });
  }
};
