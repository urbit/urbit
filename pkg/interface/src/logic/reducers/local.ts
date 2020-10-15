import _ from 'lodash';
import { StoreState } from '~/store/type';
import { Cage } from '~/types/cage';
import { LocalUpdate, BackgroundConfig } from '~/types/local-update';

type LocalState = Pick<StoreState, 'sidebarShown' | 'omniboxShown' | 'baseHash' | 'hideAvatars' | 'hideNicknames' | 'background' | 'dark' | 'suspendedFocus' | 'remoteContentPolicy'>;

export default class LocalReducer<S extends LocalState> {
    rehydrate(state: S) {
      try {
        const json = JSON.parse(localStorage.getItem('localReducer') || '{}');
        _.forIn(json, (value, key) => {
          state[key] = value;
        });
      } catch (e) {
        console.warn('Failed to rehydrate localStorage state', e);
      }
    }

    dehydrate(state: S) {
      const json = _.pick(state, ['hideNicknames' , 'hideAvatars' , 'background', 'remoteContentPolicy']);
      localStorage.setItem('localReducer', JSON.stringify(json));
    }
    reduce(json: Cage, state: S) {
        const data = json['local'];
        if (data) {
            this.sidebarToggle(data, state);
            this.setDark(data, state);
            this.baseHash(data, state);
            this.backgroundConfig(data, state)
            this.hideAvatars(data, state)
            this.hideNicknames(data, state)
            this.omniboxShown(data, state);
            this.remoteContentPolicy(data, state);
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

    backgroundConfig(obj: LocalUpdate, state: S) {
      if('backgroundConfig' in obj) {
        state.background = obj.backgroundConfig;
      }
    }

    remoteContentPolicy(obj: LocalUpdate, state: S) {
      if('remoteContentPolicy' in obj) {
        state.remoteContentPolicy = obj.remoteContentPolicy;
      }
    }

    hideAvatars(obj: LocalUpdate, state: S) {
      if('hideAvatars' in obj) {
        state.hideAvatars = obj.hideAvatars;
      }
    }

    hideNicknames(obj: LocalUpdate, state: S) {
      if( 'hideNicknames' in obj) {
        state.hideNicknames = obj.hideNicknames;
      }
    }
}
