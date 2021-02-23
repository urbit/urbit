import BaseApi from './base';
import { StoreState } from '../store/type';
import { Key,
  Value,
  Bucket
} from '@urbit/api/settings';

export default class SettingsApi extends BaseApi<StoreState> {

  async getAll() {
    const data = await this.scry('settings-store', '/all');
    this.store.handleEvent({ data: { 'settings-data': data.all } });
  }

  async getBucket(bucket: Key) {
    const data = await this.scry('settings-store', `/bucket/${bucket}`);
    this.store.handleEvent({ data: { 'settings-data': {
      'bucket-key': bucket,
      'bucket': data.bucket
    } } });
  }

  async getEntry(bucket: Key, entry: Key) {
    const data = await this.scry('settings-store', `/entry/${bucket}/${entry}`);
    this.store.handleEvent({ data: { 'settings-data': {
      'bucket-key': bucket,
      'entry-key': entry,
      'entry': data.entry
    } } });
  }
}
