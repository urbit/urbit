import _ from 'lodash';
import { StoreState } from '../../store/type';
import { Cage } from '~/types/cage';
import { LocalUpdate } from '~/types/local-update';

type LocalState = Pick<StoreState, 'sidebarShown' | 'omniboxShown' | 'dark' | 'baseHash' | 'suspendedFocus'>;

export default class LocalReducer<S extends LocalState> {
    reduce(json: Cage, state: S) {
        const data = json['local'];
        if (data) {
            this.sidebarToggle(data, state);
            this.setDark(data, state);
            this.baseHash(data, state);
            this.omniboxShown(data, state);
        }
    }
    baseHash(obj: LocalUpdate, state: S) {
      if ('baseHash' in obj) {
        state.baseHash = obj.baseHash;
      }
    }

    omniboxShown(obj: LocalUpdate, state: S) {
      if ('omniboxShown' in obj) {
          state.omniboxShown = !state.omniboxShown;
          if (state.suspendedFocus) {
            state.suspendedFocus.focus();
            state.suspendedFocus = null;
          } else {
            state.suspendedFocus = document.activeElement;
            document.activeElement?.blur();
          }
      }
    }

    sidebarToggle(obj: LocalUpdate, state: S) {
      if ('sidebarToggle' in obj) {
          state.sidebarShown = !state.sidebarShown;
      }
    }

    setDark(obj: LocalUpdate, state: S) {
      if('setDark' in obj) {
        state.dark = obj.setDark;
      }
    }
}
