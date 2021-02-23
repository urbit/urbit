import _ from 'lodash';
import { compose } from 'lodash/fp';

import { MetadataUpdate } from '@urbit/api';
import { SubscriptionRequestInterface, UrbitInterface } from '@urbit/http-api';

import useMetadataState, { MetadataState } from '~/logic/state/metadata';

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
}

export const groupInitial = (json: MetadataUpdate, state: MetadataState): MetadataState => {
  const data = _.get(json, 'initial-group', false);
  if (data) {
    state = associations(data, state);
  }
  return state;
}

export const add = (json: MetadataUpdate, state: MetadataState): MetadataState => {
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
}

export const update = (json: MetadataUpdate, state: MetadataState): MetadataState => {
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
}

export const remove = (json: MetadataUpdate, state: MetadataState): MetadataState => {
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
}

export const metadataReducer = (message) => {
  console.log('received metadata', message);
  useMetadataState.setState(
    compose([
      associations,
      add,
      update,
      remove,
      groupInitial,
    ].map(reducer => reducer.bind(reducer, message['metadata-update']))
    )(useMetadataState.getState())
  );
};

const metadataSubscription = (channel: UrbitInterface): SubscriptionRequestInterface => {
  const event = metadataReducer;
  const err = (message) => {
    console.error(message);
    channel.subscribe(metadataSubscription(channel));
  };
  const quit = (message) => {
    console.error(message);
    channel.subscribe(metadataSubscription(channel));
  };
  return {
    app: 'metadata-store',
    path: '/all',
    event, err, quit
  };
};

export default metadataSubscription;