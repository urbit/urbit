import _ from 'lodash';

export class MetadataReducer {
  reduce(json, state) {
    let data = _.get(json, 'metadata-update', false);
    if (data) {
      this.associations(data, state);
      this.add(data, state);
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
      if (data['app-name'] !== 'link') {
        return;
      }
      if (state.resources[data['app-path']]) {
        console.error('beware! overwriting previous data', data['app-path']);
      }
      state.resources[data['app-path']] = {
        group: data['group-path'],
        ...data.metadata
      };
    }
  }
}
