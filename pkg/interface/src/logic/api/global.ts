import { Patp } from '@urbit/api';
import GlobalStore from '../store/store';
import { StoreState } from '../store/type';
import BaseApi from './base';
import ContactsApi from './contacts';
import GcpApi from './gcp';
import GraphApi from './graph';
import GroupsApi from './groups';
import { HarkApi } from './hark';
import InviteApi from './invite';
import LaunchApi from './launch';
import LocalApi from './local';
import MetadataApi from './metadata';
import S3Api from './s3';
import SettingsApi from './settings';
import TermApi from './term';

export default class GlobalApi extends BaseApi<StoreState> {
  local = new LocalApi(this.ship, this.channel, this.store);
  invite = new InviteApi(this.ship, this.channel, this.store);
  metadata = new MetadataApi(this.ship, this.channel, this.store);
  contacts = new ContactsApi(this.ship, this.channel, this.store);
  groups = new GroupsApi(this.ship, this.channel, this.store);
  launch = new LaunchApi(this.ship, this.channel, this.store);
  gcp = new GcpApi(this.ship, this.channel, this.store);
  s3 = new S3Api(this.ship, this.channel, this.store);
  graph = new GraphApi(this.ship, this.channel, this.store);
  hark = new HarkApi(this.ship, this.channel, this.store);
  settings = new SettingsApi(this.ship, this.channel, this.store);
  term = new TermApi(this.ship, this.channel, this.store);

  constructor(
    public ship: Patp,
    public channel: any,
    public store: GlobalStore
  ) {
    super(ship, channel, store);
  }
}

