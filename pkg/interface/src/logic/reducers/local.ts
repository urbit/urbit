import _ from 'lodash';
import { StoreState } from '~/store/type';
import { Cage } from '~/types/cage';
import { LocalUpdate } from '~/types/local-update';

type LocalState = Pick<StoreState, 'baseHash'>;

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
    }
    reduce(json: Cage, state: S) {
        const data = json['local'];
        if (data) {
            this.baseHash(data, state);
        }
    }
    baseHash(obj: LocalUpdate, state: S) {
      if ('baseHash' in obj) {
        state.baseHash = obj.baseHash;
      }
    }
}
