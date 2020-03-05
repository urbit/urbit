import _ from 'lodash';

export class MetadataReducer {
  reduce(json, state) {
    let data = _.get(json, 'metadata-update', false);
    if (data) {
      this.associations(data, state);
      this.add(data, state);
      this.update(data, state);
      this.remove(data, state);
    }
  }

  associations(json, state) {
    let data = _.get(json, 'associations', false);
    if (data) {
      let metadata = new Map;
      Object.keys(data).map((channel) => {
        let channelObj = data[channel];
        metadata.set(channelObj["app-path"], channelObj);
      })
      state.associations = metadata;
    }
  }

  add(json, state) {
    let data = _.get(json, 'add', false);
    if (data) {
      let metadata = state.associations;
      metadata.set(data["app-path"], data);
      state.associations = metadata;
    }
  }

  update(json, state) {
    let data = _.get(json, 'update-metadata', false);
    if (data) {
      let metadata = state.associations;
      metadata.set(data["app-path"], data);
      state.associations = metadata;
    }
  }

  remove(json, state) {
    let data = _.get(json, 'remove', false);
    if (data) {
      let metadata = state.associations;
      metadata.delete(data["app-path"]);
      state.associations = metadata;
    }
  }
}