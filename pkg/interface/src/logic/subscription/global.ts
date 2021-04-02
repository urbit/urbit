import BaseSubscription from './base';
import { StoreState } from '../store/type';
import { Path } from '@urbit/api';
import _ from 'lodash';


export default class GlobalSubscription extends BaseSubscription<StoreState> {
  openSubscriptions: any = {};

  start() {
    this.subscribe('/all', 'metadata-store');
    this.subscribe('/all', 'invite-store');
    this.subscribe('/all', 'launch');
    this.subscribe('/all', 'weather');
    this.subscribe('/groups', 'group-store');
    this.clearQueue();

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
    for (let key in Object.keys(this.openSubscriptions)) {
      let val = this.openSubscriptions[key];
      if (id === val.id) {
        delete this.openSubscriptions[`${val.app}${val.path}`];
        super.unsubscribe(id);
      }
    }
  }

  restart() {
    for (let key in Object.keys(this.openSubscriptions)) {
      let val = this.openSubscriptions[key];

      this.unsubscribe(val.id);
    }

    this.start();
  }
}
