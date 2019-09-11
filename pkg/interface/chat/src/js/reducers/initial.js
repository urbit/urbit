import _ from 'lodash';


export class InitialReducer {
  reduce(json, state) {
    let data = _.get(json, 'inbox-initial', false);
    if (data) {
      state.inbox = data;
    }

    data = _.get(json, 'group-initial', false);
    if (data) {
      for (let group in data) {
        state.groups[group] = new Set(data[group]);
      }
    }

    data = _.get(json, 'permission-initial', false);
    if (data) {
      for (let perm in data) {
        state.permissions[perm] = {
          who: new Set(data[perm].who),
          kind: data[perm].kind
        }
      }
    }

  }
}

