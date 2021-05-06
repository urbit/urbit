import type { StoreState } from '../store/type';
import BaseApi from './base';

export default class GcpApi extends BaseApi<StoreState> {
  // Does not touch the store; use the value manually.
  async isConfigured(): Promise<boolean> {
    return this.spider('noun', 'json', 'gcp-is-configured', {});
  }

  // Does not return the token; read it out of the store.
  async getToken(): Promise<void> {
    return this.spider('noun', 'gcp-token', 'gcp-get-token', {})
      .then((token) => {
        this.store.handleEvent({
          data: token
        });
      });
  }
}
