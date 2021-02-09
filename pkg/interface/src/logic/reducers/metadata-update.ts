import _ from 'lodash';

import { StoreState } from '../../store/type';

import { MetadataUpdate } from '~/types/metadata-update';
import { Cage } from '~/types/cage';

type MetadataState = Pick<StoreState, 'associations'>;

export default class MetadataReducer<S extends MetadataState> {
  reduce(json: Cage, state: S) {
    let data = json['metadata-update']
    if (data) {
      console.log(data);
      this.associations(data, state);
      this.add(data, state);
      this.update(data, state);
      this.remove(data, state);
      this.groupInitial(data, state);
    }
  }

  groupInitial(json: MetadataUpdate, state: S) {
    const data = _.get(json, 'initial-group', false);
    console.log(data);
    if(data) {
      this.associations(data, state);
    }
  }

  associations(json: MetadataUpdate, state: S) {
    let data = _.get(json, 'associations', false);
    if (data) {
      let metadata = state.associations;
      Object.keys(data).forEach((key) => {
        let val = data[key];
        let appName = val['app-name'];
        let rid = val.resource;
        if (!(appName in metadata)) {
          metadata[appName] = {};
        }
        if (!(rid in metadata[appName])) {
          metadata[appName][rid] = {};
        }
        metadata[appName][rid] = val;
      });

      state.associations = metadata;
    }
  }

  add(json: MetadataUpdate, state: S) {
    let data = _.get(json, 'add', false);
    if (data) {
      let metadata = state.associations;
      let appName = data['app-name'];
      let appPath = data.resource;

      if (!(appName in metadata)) {
        metadata[appName] = {};
      }
      if (!(appPath in metadata[appName])) {
        metadata[appName][appPath] = {};
      }
      metadata[appName][appPath] = data;

      state.associations = metadata;
    }
  }

  update(json: MetadataUpdate, state: S) {
    let data = _.get(json, 'update-metadata', false);
    if (data) {
      let metadata = state.associations;
      let appName = data['app-name'];
      let rid = data.resource;

      if (!(appName in metadata)) {
        metadata[appName] = {};
      }
      if (!(rid in metadata[appName])) {
        metadata[appName][rid] = {};
      }
      metadata[appName][rid] = data;

      state.associations = metadata;
    }
  }

  remove(json: MetadataUpdate, state: S) {
    let data = _.get(json, 'remove', false);
    if (data) {
      let metadata = state.associations;
      let appName = data['app-name'];
      let rid = data.resource;

      if (appName in metadata && rid in metadata[appName]) {
        delete metadata[appName][rid];
      }
      state.associations = metadata;
    }
  }
}
