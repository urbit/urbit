import { Patp } from '@urbit/api';
import BaseApi from './base';
import { StoreState } from '../store/type';
import GlobalStore from '../store/store';
import LocalApi from './local';
import LaunchApi from './launch';
import S3Api from './s3';
import SettingsApi from './settings';

export default class GlobalApi extends BaseApi<StoreState> {
  local = new LocalApi(this.ship, this.channel, this.store);
  launch = new LaunchApi(this.ship, this.channel, this.store);
  s3 = new S3Api(this.ship, this.channel, this.store);
  settings = new SettingsApi(this.ship, this.channel, this.store);

  constructor(
    public ship: Patp,
    public channel: any,
    public store: GlobalStore
  ) {
    super(ship, channel, store);
  }
}

