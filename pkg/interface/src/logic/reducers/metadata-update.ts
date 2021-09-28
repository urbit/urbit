import { MetadataUpdate } from '@urbit/api/metadata';
import _ from 'lodash';
import { Cage } from '~/types/cage';
import { BaseState } from '../state/base';
import { MetadataState as State } from '../state/metadata';

type MetadataState = State & BaseState<State>;

export default class MetadataReducer {
  reduce(json: Cage) {
    return;
  }
}

const associations = (json: MetadataUpdate, state: MetadataState): MetadataState => {
  const data = _.get(json, 'associations', false);
  if (data) {
    const metadata = state.associations;
    Object.keys(data).forEach((key) => {
      const val = data[key];
      const appName = val['app-name'];
      const rid = val.resource;
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
  return state;
};

const add = (json: MetadataUpdate, state: MetadataState): MetadataState => {
  const data = _.get(json, 'add', false);
  if (data) {
    const metadata = state.associations;
    const appName = data['app-name'];
    const appPath = data.resource;

    if (!(appName in metadata)) {
      metadata[appName] = {};
    }
    if (!(appPath in metadata[appName])) {
      metadata[appName][appPath] = {};
    }
    metadata[appName][appPath] = data;

    state.associations = metadata;
  }
  return state;
};

const groupInitial = (json: MetadataUpdate, state: MetadataState): MetadataState => {
  const data = _.get(json, 'initial-group', false);
  if(data) {
    associations(data, state);
  }
  return state;
};

const update = (json: MetadataUpdate, state: MetadataState): MetadataState => {
  const data = _.get(json, 'update-metadata', false);
  if (data) {
    const metadata = state.associations;
    const appName = data['app-name'];
    const rid = data.resource;

    if (!(appName in metadata)) {
      metadata[appName] = {};
    }
    if (!(rid in metadata[appName])) {
      metadata[appName][rid] = {};
    }
    metadata[appName][rid] = data;

    state.associations = metadata;
  }
  return state;
};

const remove = (json: MetadataUpdate, state: MetadataState): MetadataState => {
  const data = _.get(json, 'remove', false);
  if (data) {
    const metadata = state.associations;
    const appName = data['app-name'];
    const rid = data.resource;

    if (appName in metadata && rid in metadata[appName]) {
      delete metadata[appName][rid];
    }
    state.associations = metadata;
  }
  return state;
};

export const reduce = [
  associations,
  add,
  update,
  remove,
  groupInitial
];

