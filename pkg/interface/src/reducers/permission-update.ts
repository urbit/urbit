import _ from 'lodash';
import { StoreState } from '../store/type';
import { Cage } from '../types/cage';
import { PermissionUpdate } from '../types/permission-update';

type PermissionState = Pick<StoreState, "permissions">;

export default class PermissionReducer<S extends PermissionState> {
  reduce(json: Cage, state: S) {
    const data = _.get(json, 'permission-update', false);
    if (data) {
      this.initial(data, state);
      this.create(data, state);
      this.delete(data, state);
      this.add(data, state);
      this.remove(data, state);
    }
  }

  initial(json: PermissionUpdate, state: S) {
    const data = _.get(json, 'initial', false);
    if (data) {
      for (const perm in data) {
        state.permissions[perm] = {
          who: new Set(data[perm].who),
          kind: data[perm].kind
        };
      }
    }
  }

  create(json: PermissionUpdate, state: S) {
    const data = _.get(json, 'create', false);
    if (data) {
      state.permissions[data.path] = {
        kind: data.kind,
        who: new Set(data.who)
      };
    }
  }

  delete(json: PermissionUpdate, state: S) {
    const data = _.get(json, 'delete', false);
    if (data) {
      delete state.permissions[data.path];
    }
  }

  add(json: PermissionUpdate, state: S) {
    const data = _.get(json, 'add', false);
    if (data) {
      for (const member of data.who) {
        state.permissions[data.path].who.add(member);
      }
    }
  }

  remove(json: PermissionUpdate, state: S) {
    const data = _.get(json, 'remove', false);
    if (data) {
      for (const member of data.who) {
        state.permissions[data.path].who.delete(member);
      }
    }
  }
}
