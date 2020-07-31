import { StoreState } from '../store/type';
import { Cage } from '../types/cage';
import { LinkListenUpdate } from '../types/link-listen-update';

type LinkListenState = Pick<StoreState, 'linkListening'>;

export default class LinkListenReducer<S extends LinkListenState> {
  reduce(json: Cage, state: S) {
    const data = json['link-listen-update'] || false;
    if (data) {
      this.listening(data, state);
      this.watch(data, state);
      this.leave(data, state);
    }
  }

  listening(json: LinkListenUpdate, state: S) {
    const data = json['listening'] || false;
    if (data) {
      state.linkListening = new Set(data);
    }
  }

  watch(json: LinkListenUpdate, state: S) {
    const data = json['watch'] || false;
    if (data) {
      state.linkListening.add(data);
    }
  }

  leave(json: LinkListenUpdate, state: S) {
    const data = json['leave'] || false;
    if (data) {
      state.linkListening.delete(data);
    }
  }
}

