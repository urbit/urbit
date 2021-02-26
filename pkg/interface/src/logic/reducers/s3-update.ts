import _ from 'lodash';
import { compose } from 'lodash/fp';
import { Cage } from '~/types/cage';
import { S3Update } from '~/types/s3-update';
import useS3State, { S3State } from '../state/s3';


export default class S3Reducer {
  reduce(json: Cage) {
    const data = _.get(json, 's3-update', false);
    if (data) {
      useS3State.setState(
        compose([
          credentials,
          configuration,
          currentBucket,
          addBucket,
          removeBucket,
          endpoint,
          accessKeyId,
          secretAccessKey,
        ].map(reducer => reducer.bind(reducer, data))
        )(useS3State.getState())
      )
    }
  }
}

const credentials = (json: S3Update, state: S3State): S3State => {
  const data = _.get(json, 'credentials', false);
  if (data) {
    state.credentials = data;
  }
  return state;
}

const configuration = (json: S3Update, state: S3State): S3State => {
  const data = _.get(json, 'configuration', false);
  if (data) {
    state.configuration = {
      buckets: new Set(data.buckets),
      currentBucket: data.currentBucket
    };
  }
  return state;
}

const currentBucket = (json: S3Update, state: S3State): S3State => {
  const data = _.get(json, 'setCurrentBucket', false);
  if (data && state.s3) {
    state.configuration.currentBucket = data;
  }
  return state;
}

const addBucket = (json: S3Update, state: S3State): S3State => {
  const data = _.get(json, 'addBucket', false);
  if (data) {
    state.configuration.buckets =
      state.configuration.buckets.add(data);
  }
  return state;
}

const removeBucket = (json: S3Update, state: S3State): S3State => {
  const data = _.get(json, 'removeBucket', false);
  if (data) {
    state.configuration.buckets.delete(data);
  }
  return state;
}

const endpoint = (json: S3Update, state: S3State): S3State => {
  const data = _.get(json, 'setEndpoint', false);
  if (data && state.credentials) {
    state.credentials.endpoint = data;
  }
  return state;
}

const accessKeyId = (json: S3Update , state: S3State): S3State => {
  const data = _.get(json, 'setAccessKeyId', false);
  if (data && state.credentials) {
    state.credentials.accessKeyId = data;
  }
  return state;
}

const secretAccessKey = (json: S3Update, state: S3State): S3State => {
  const data = _.get(json, 'setSecretAccessKey', false);
  if (data && state.credentials) {
    state.credentials.secretAccessKey = data;
  }
  return state;
}