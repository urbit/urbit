import { Patp } from '../types/noun';
import BaseApi from './base';
import ChatApi from './chat';
import { StoreState } from '../store/type';
import GlobalStore from '../store/store';
import LocalApi from './local';
import InviteApi from './invite';
import MetadataApi from './metadata';
import ContactsApi from './contacts';
import GroupsApi from './groups';
import LaunchApi from './launch';
import LinksApi from './links';
import PublishApi from './publish';

export default class GlobalApi extends BaseApi<StoreState> {
  chat = new ChatApi(this.ship, this.channel, this.store);
  local = new LocalApi(this.ship, this.channel, this.store);
  invite = new InviteApi(this.ship, this.channel, this.store);
  metadata = new MetadataApi(this.ship, this.channel, this.store);
  contacts = new ContactsApi(this.ship, this.channel, this.store);
  groups = new GroupsApi(this.ship, this.channel, this.store);
  launch = new LaunchApi(this.ship, this.channel, this.store);
  links = new LinksApi(this.ship, this.channel, this.store);
  publish = new PublishApi(this.ship, this.channel, this.store);


  constructor(public ship: Patp, public channel: any, public store: GlobalStore) {
    super(ship,channel,store);
  }
}

