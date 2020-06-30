import BaseSubscription from './base';
import { StoreState } from '../store/type';
import { Path } from '../types/noun';


/**
 * Path to subscribe on and app to subscribe to
 */
type AppSubscription = [Path, string];

const chatSubscriptions: AppSubscription[] = [
  ['/primary', 'chat-view'],
  ['/synced', 'chat-hook']
];

const publishSubscriptions: AppSubscription[] = [
  ['/primary', 'publish'],
  ['/all', 'group-store']
];

const linkSubscriptions: AppSubscription[] = [
  ['/json/seen', 'link-view'],
  ['/listening', 'link-listen-hook']
]

const groupSubscriptions: AppSubscription[] = [
  ['/all', 'group-store'],
  ['/synced', 'contact-hook']
];

const walletSubscriptions: AppSubscription[] = [
  ['/primary', 'wallet']
]

type AppName = 'publish' | 'chat' | 'link' | 'groups' | 'wallet';
const appSubscriptions: Record<AppName, AppSubscription[]> = {
  chat: chatSubscriptions,
  publish: publishSubscriptions,
  link: linkSubscriptions,
  groups: groupSubscriptions,
  wallet: walletSubscriptions
};

export default class GlobalSubscription extends BaseSubscription<StoreState> {
  openSubscriptions: Record<AppName, number[]> = {
    chat: [],
    publish: [],
    link: [],
    groups: [],
    wallet: []
  };
  start() {
    this.subscribe('/all', 'invite-store');
    this.subscribe('/all', 'permission-store');
    this.subscribe('/primary', 'contact-view');
    this.subscribe('/all', 'metadata-store');
    this.subscribe('/all', 's3-store');
    this.subscribe('/all', 'launch');
    this.subscribe('/all', 'weather');
  }

  startApp(app: AppName) {
    if(this.openSubscriptions[app].length > 0) {
      console.log(`${app} already started`);
      return;
    }
    this.openSubscriptions[app] = appSubscriptions[app].map(([path, agent]) => this.subscribe(path, agent));
  }

  stopApp(app: AppName) {
    this.openSubscriptions[app].map(id => this.unsubscribe(id))
    this.openSubscriptions[app] = [];
  }
}
