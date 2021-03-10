import _ from 'lodash';
import { StoreState } from '../../store/type';
import { Cage } from '~/types/cage';
import { S3Update } from '~/types/s3-update';

type S3State = Pick<StoreState, 's3'>;

export default class S3Reducer<S extends S3State> {
  reduce(json: Cage, state: S) {
    const data = _.get(json, 's3-update', false);
    if (data) {
      this.credentials(data, state);
      this.configuration(data, state);
      this.currentBucket(data, state);
      this.addBucket(data, state);
      this.removeBucket(data, state);
      this.endpoint(data, state);
      this.accessKeyId(data, state);
      this.secretAccessKey(data, state);
    }
  }

  credentials(json: S3Update, state: S) {
    const data = _.get(json, 'credentials', false);
    if (data) {
      state.storage.s3.credentials = data;
    }
  }

  configuration(json: S3Update, state: S) {
    const data = _.get(json, 'configuration', false);
    if (data) {
      state.storage.s3.configuration = {
        buckets: new Set(data.buckets),
        currentBucket: data.currentBucket
      };
    }
  }

  currentBucket(json: S3Update, state: S) {
    const data = _.get(json, 'setCurrentBucket', false);
    if (data && state.storage.s3) {
      state.storage.s3.configuration.currentBucket = data;
    }
  }

  addBucket(json: S3Update, state: S) {
    const data = _.get(json, 'addBucket', false);
    if (data) {
      state.storage.s3.configuration.buckets =
        state.storage.s3.configuration.buckets.add(data);
    }
  }

  removeBucket(json: S3Update, state: S) {
    const data = _.get(json, 'removeBucket', false);
    if (data) {
      state.storage.s3.configuration.buckets.delete(data);
    }
  }

  endpoint(json: S3Update, state: S) {
    const data = _.get(json, 'setEndpoint', false);
    if (data && state.storage.s3.credentials) {
      state.storage.s3.credentials.endpoint = data;
    }
  }

  accessKeyId(json: S3Update , state: S) {
    const data = _.get(json, 'setAccessKeyId', false);
    if (data && state.storage.s3.credentials) {
      state.storage.s3.credentials.accessKeyId = data;
    }
  }

  secretAccessKey(json: S3Update, state: S) {
    const data = _.get(json, 'setSecretAccessKey', false);
    if (data && state.storage.s3.credentials) {
      state.storage.s3.credentials.secretAccessKey = data;
    }
  }
}

