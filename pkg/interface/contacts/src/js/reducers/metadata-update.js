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
      Object.keys(data).map((channel) => {
        let channelObj = data[channel];
        if (metadata.has(channelObj["group-path"])) {
          let groupMetadata = metadata.get(channelObj["group-path"]);
          groupMetadata[channel] = channelObj;
          metadata.set(channelObj["group-path"], groupMetadata);
        } else {
          metadata.set(channelObj["group-path"], {[channel]: channelObj});
        }
      })
      state.associations = metadata;
    }
  }

  add(json, state) {
    let data = _.get(json, 'add', false);
    if (data) {
      let metadata = state.associations;
      if (metadata.has(data["group-path"])) {
        let groupMetadata = metadata.get(data["group-path"]);
        groupMetadata[`${data["group-path"]}/${data["app-name"]}${data["app-path"]}`] = data;
      } else {
        metadata.set(data["group-path"], data);
      }
      state.associations = metadata;
    }
  }
}