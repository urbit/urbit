import _ from 'lodash';

export default class MetadataReducer {
  reduce(json, state) {
    let data = _.get(json, 'metadata-update', false);
    if (data) {
      console.log(data);
      this.associations(data, state);
      this.add(data, state);
      this.update(data, state);
      this.remove(data, state);
    }
  }

  associations(json, state) {
    let data = _.get(json, 'associations', false);
    if (data) {
      let metadata = state.associations;
      Object.keys(data).forEach((key) => {
        let val = data[key];
        let appName = val['app-name'];
        let groupPath = val['group-path'];
        if (!(appName in metadata)) {
          metadata[appName] = {};
        }
        if (!(groupPath in metadata[appName])) {
          metadata[appName][groupPath] = {};
        }
        metadata[appName][groupPath] = val;
      });
      state.associations = metadata;
    }
  }

  add(json, state) {
    let data = _.get(json, 'add', false);
    if (data) {
      let metadata = state.associations;
      if (!(data['app-name'] in metadata)) {
        metadata[data['app-name']] = {};
      }
      if (!(data['group-path'] in metadata[data['app-name']])) {
        metadata[data['app-name']][data['group-path']] = {};
      }
      metadata[data['app-name']][data['group-path']] = data;

      state.associations = metadata;
    }
  }

  update(json, state) {
    let data = _.get(json, 'update-metadata', false);
    if (data) {
      let metadata = state.associations;
      if (!(data['app-name'] in metadata)) {
        metadata[data['app-name']] = {};
      }
      if (!(data['group-path'] in metadata[data['app-name']])) {
        metadata[data['app-name']][data['group-path']] = {};
      }
      metadata[data['app-name']][data['group-path']] = data;

      state.associations = metadata;
    }
  }

  remove(json, state) {
    let data = _.get(json, 'remove', false);
    if (data) {
      let metadata = state.associations;
      if (data['group-path'] in metadata) {
        let path =
        `${data['group-path']}/${data['app-name']}${data['app-path']}`
        delete metadata[data["group-path"]][path];
        state.associations = metadata;
      }
    }
  }
}
