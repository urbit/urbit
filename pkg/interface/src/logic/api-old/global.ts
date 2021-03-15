import { Patp } from '@urbit/api';
import BaseApi from './base';
import { StoreState } from '../store/type';
import GlobalStore from '../store/store';
import LocalApi from './local';
import GroupsApi from './groups';
import LaunchApi from './launch';
import GraphApi from './graph';
import S3Api from './s3';
import GcpApi from './gcp';
import { HarkApi } from './hark';
import SettingsApi from './settings';

export default class GlobalApi extends BaseApi<StoreState> {
  local = new LocalApi(this.ship, this.channel, this.store);
  groups = new GroupsApi(this.ship, this.channel, this.store);
  launch = new LaunchApi(this.ship, this.channel, this.store);
  gcp = new GcpApi(this.ship, this.channel, this.store);
  s3 = new S3Api(this.ship, this.channel, this.store);
  graph = new GraphApi(this.ship, this.channel, this.store);
  hark = new HarkApi(this.ship, this.channel, this.store);
  settings = new SettingsApi(this.ship, this.channel, this.store);

  constructor(
    public ship: Patp,
    public channel: any,
    public store: GlobalStore
  ) {
    super(ship, channel, store);
  }
}

