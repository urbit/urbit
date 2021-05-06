import { StoreState } from '../store/type';
import BaseApi from './base';

export default class LocalApi extends BaseApi<StoreState> {
  getBaseHash() {
    this.scry<string>('file-server', '/clay/base/hash').then((baseHash) => {
      this.store.handleEvent({ data: { baseHash } });
    });
  }
}
