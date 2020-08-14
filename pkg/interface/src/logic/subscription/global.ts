import BaseSubscription from './base';
import { StoreState } from '../store/type';
import { Path } from '../types/noun';
import _ from 'lodash';


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
];

const linkSubscriptions: AppSubscription[] = [
  ['/json/seen', 'link-view'],
  ['/listening', 'link-listen-hook']
]

const groupSubscriptions: AppSubscription[] = [
  ['/synced', 'contact-hook']
];

type AppName = 'publish' | 'chat' | 'link' | 'groups';
const appSubscriptions: Record<AppName, AppSubscription[]> = {
  chat: chatSubscriptions,
  publish: publishSubscriptions,
  link: linkSubscriptions,
  groups: groupSubscriptions
};

export default class GlobalSubscription extends BaseSubscription<StoreState> {
  openSubscriptions: Record<AppName, number[]> = {
    chat: [],
    publish: [],
    link: [],
    groups: []
  };
  start() {
    this.subscribe('/all', 'invite-store');
    this.subscribe('/groups', 'group-store');
    this.subscribe('/primary', 'contact-view');
    this.subscribe('/all', 'metadata-store');
    this.subscribe('/all', 's3-store');
    this.subscribe('/all', 'launch');
    this.subscribe('/all', 'weather');
  }

  restart() {
    super.restart();
    _.mapValues(this.openSubscriptions, (subs, app: AppName) => {
      if(subs.length > 0) {
        this.stopApp(app);
        this.startApp(app);
      }
    });
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
