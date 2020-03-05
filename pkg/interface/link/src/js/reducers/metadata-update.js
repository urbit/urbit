import _ from 'lodash';

export class MetadataReducer {
  reduce(json, state) {
    let data = _.get(json, 'metadata-update', false);
    if (data) {
      this.associations(data, state);
      this.add(data, state);
      this.remove(data, state);
      this.update(data, state);
    }
  }

  associations(json, state) {
    let data = _.get(json, 'associations', false);
    if (data) {
      let metadata = new Map;
      Object.keys(data).map((key) => {
        let assoc = data[key];
        if (assoc['app-name'] !== 'link') {
          return;
        }
        if (state.resources[assoc['app-path']]) {
          console.error('beware! overwriting previous data', data['app-path']);
        }
        state.resources[assoc['app-path']] = {
          group: assoc['group-path'],
          ...assoc.metadata
        };
      });
    }
  }

  add(json, state) {
    let data = _.get(json, 'add', false);
    if (data) {
      if (state.resources[data['app-path']]) {
        console.error('beware! overwriting previous data', data['app-path']);
      }
      this.update({'update-metadata': data}, state);
    }
  }

  remove(json, state) {
    let data = _.get(json, 'remove', false);
    if (data) {
      if (data['app-name'] !== 'link') {
        return;
      }
      const have = state.resources[data['app-path']];
      if (have && have.group === data['group-path']) {
        delete state.resources[data['app-path']];
      }
    }
  }

  update(json, state) {
    let data = _.get(json, 'update-metadata', false);
    if (data) {
      if (data['app-name'] !== 'link') {
        return;
      }
      state.resources[data['app-path']] = {
        group: data['group-path'],
        ...data.metadata
      };
    }
  }
}
