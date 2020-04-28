import _ from 'lodash';

export default class MetadataReducer {
  reduce(json, state) {
    const data = _.get(json, 'metadata-update', false);
    if (data) {
      this.associations(data, state);
      this.add(data, state);
      this.update(data, state);
      this.remove(data, state);
    }
  }

  associations(json, state) {
    const data = _.get(json, 'associations', false);
    if (data) {
      const metadata = state.associations;
      Object.keys(data).map((channel) => {
        const channelObj = data[channel];
        const app = data[channel]['app-name'];
        if (!(app in metadata)) {
          metadata[app] = {};
        }
        metadata[app][channelObj['app-path']] =  channelObj;
      });
      state.associations = metadata;
    }
  }

  add(json, state) {
    const data = _.get(json, 'add', false);
    if (data) {
      const metadata = state.associations;
      const app = data['app-name'];
      if (!(app in metadata)) {
        metadata[app] = {};
      }
      metadata[app][data['app-path']] = data;
      state.associations = metadata;
    }
  }

  update(json, state) {
    const data = _.get(json, 'update-metadata', false);
    if (data) {
      const metadata = state.associations;
      const app = data['app-name'];
      metadata[app][data['app-path']] = data;
      state.associations = metadata;
    }
  }

  remove(json, state) {
    const data = _.get(json, 'remove', false);
    if (data) {
      const metadata = state.associations;
      const app = data['app-name'];
      if (!(app in metadata)) {
        return false;
      }
      delete metadata[app][data['app-path']];
      state.associations = metadata;
    }
  }
}
