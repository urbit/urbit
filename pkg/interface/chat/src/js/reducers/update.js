import _ from 'lodash';


export class UpdateReducer {
  reduce(json, state) {
    console.log('update', json);
    let data = _.get(json, 'update', false);
    if (data) {
      this.add(data, state);
      this.remove(data, state);
      this.bundle(data, state);
      this.unbundle(data, state);
      this.keys(data, state);
      this.path(data, state);
    }
  }

  add(json, state) {
    let data = _.get(json, 'add', false);
    if (data) {
      for (let member of data.members) {
        state.groups[data.path].add(member);
      }
    }
  }

  remove(json, state) {
    let data = _.get(json, 'remove', false);
    if (data) {
      for (let member of data.members) {
        state.groups[data.path].delete(member);
      }
    }
  }

  bundle(json, state) {

    let data = _.get(json, 'bundle', false);
    if (data) {
      state.groups[data.path] = new Set();
    }
  }

  unbundle(json, state) {
    let data = _.get(json, 'unbundle', false);
    if (data) {
      delete state.groups[data.path];
    }
  }

  keys(json, state) {
    let data = _.get(json, 'keys', false);
    if (data) {
      state.groupKeys = new Set(data.keys);
    }
  }

  path(json, state) {
    let data = _.get(json, 'path', false);
    if (data) {
      state.groups[data.path] = new Set([data.members]);
    }
  }

}

