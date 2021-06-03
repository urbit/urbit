import { Path } from '@urbit/api';
import { StoreState } from '../store/type';
import BaseSubscription from './base';

export default class GlobalSubscription extends BaseSubscription<StoreState> {
  openSubscriptions: any = {};

  start() {
    this.subscribe('/all', 'metadata-store');
    this.subscribe('/all', 'invite-store');
    this.subscribe('/all', 'launch');
    this.subscribe('/all', 'weather');
    this.subscribe('/groups', 'group-store');
    this.clearQueue();

    this.subscribe('/updates', 'dm-hook');
    this.subscribe('/all', 'contact-store');
    this.subscribe('/all', 's3-store');
    this.subscribe('/keys', 'graph-store');
    this.subscribe('/updates', 'hark-store');
    this.subscribe('/updates', 'hark-graph-hook');
    this.subscribe('/updates', 'hark-group-hook');
    this.subscribe('/all', 'settings-store');
    this.subscribe('/all', 'group-view');
    this.subscribe('/nacks', 'contact-pull-hook');
    this.clearQueue();

    this.subscribe('/updates', 'graph-store');
  }

  subscribe(path: Path, app: string) {
    if (`${app}${path}` in this.openSubscriptions) {
      return;
    }

    const id = super.subscribe(path, app);
    this.openSubscriptions[`${app}${path}`] = { app, path, id };
  }

  unsubscribe(id) {
    for (const key in Object.keys(this.openSubscriptions)) {
      const val = this.openSubscriptions[key];
      if (id === val.id) {
        delete this.openSubscriptions[`${val.app}${val.path}`];
        super.unsubscribe(id);
      }
    }
  }

  restart() {
    this.openSubscriptions = {};
    super.restart();
  }
}
