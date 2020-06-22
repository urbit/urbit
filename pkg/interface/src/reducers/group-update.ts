import _ from 'lodash';
import { StoreState } from '../store/type';
import { Cage } from '../types/cage';
import { GroupUpdate } from '../types/group-update';

type GroupState = Pick<StoreState, 'groups' | 'groupKeys'>;

export default class GroupReducer<S extends GroupState>  {

  reduce(json: Cage, state: S) {
    const data = _.get(json, "group-update", false);
    if (data) {
      this.initial(data, state);
      this.add(data, state);
      this.remove(data, state);
      this.bundle(data, state);
      this.unbundle(data, state);
      this.keys(data, state);
      this.path(data, state);
    }
  }

  initial(json: GroupUpdate, state: S) {
    const data = _.get(json, 'initial', false);
    if (data) {
      for (let group in data) {
       state.groups[group] = new Set(data[group]);
      }
    }
  }

  add(json: GroupUpdate, state: S) {
    const data = _.get(json, 'add', false);
    if (data) {
      for (const member of data.members) {
        state.groups[data.path].add(member);
      }
    }
  }

  remove(json: GroupUpdate, state: S) {
    const data = _.get(json, 'remove', false);
    if (data) {
      for (const member of data.members) {
        state.groups[data.path].delete(member);
      }
    }
  }

  bundle(json: GroupUpdate, state: S) {
    const data = _.get(json, 'bundle', false);
    if (data) {
      state.groups[data.path] = new Set();
    }
  }

  unbundle(json: GroupUpdate, state: S) {
    const data = _.get(json, 'unbundle', false);
    if (data) {
      delete state.groups[data.path];
    }
  }

  keys(json: GroupUpdate, state: S) {
    const data = _.get(json, 'keys', false);
    if (data) {
      state.groupKeys = new Set(data.keys);
    }
  }

  path(json, state) {
    const data = _.get(json, 'path', false);
    if (data) {
      state.groups[data.path] = new Set([data.members]);
    }
  }
}

