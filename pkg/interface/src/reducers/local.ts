import _ from 'lodash';
import { StoreState } from '../store/type';
import { Cage } from '../types/cage';
import { LocalUpdate } from '../types/local-update';

type LocalState = Pick<StoreState, 'sidebarShown' | 'selectedGroups' | 'dark' | 'baseHash'>;

export default class LocalReducer<S extends LocalState> {
    reduce(json: Cage, state: S) {
        const data = json['local'];
        if (data) {
            this.sidebarToggle(data, state);
            this.setSelected(data, state);
            this.setDark(data, state);
            this.baseHash(data, state);
        }
    }
    baseHash(obj: LocalUpdate, state: S) {
      if ('baseHash' in obj) {
        state.baseHash = obj.baseHash;
      }
    }

    sidebarToggle(obj: LocalUpdate, state: S) {
      if ('sidebarToggle' in obj) {
          state.sidebarShown = !state.sidebarShown;
      }
    }

    setSelected(obj: LocalUpdate, state: S) {
      if ('selected' in obj) {
        state.selectedGroups = obj.selected;
      }
    }

    setDark(obj: LocalUpdate, state: S) {
      if('setDark' in obj) {
        state.dark = obj.setDark;
      }
    }
}
