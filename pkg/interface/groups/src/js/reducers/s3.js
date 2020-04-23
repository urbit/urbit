import _ from 'lodash';


export class S3Reducer {
  reduce(json, state) {
    let data = _.get(json, 's3-update', false);
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

  credentials(json, state) {
    let data = _.get(json, 'credentials', false);
    if (data) {
      state.s3.credentials = data;
    }
  }

  configuration(json, state) {
    let data = _.get(json, 'configuration', false);
    if (data) {
      state.s3.configuration = {
        buckets: new Set(data.buckets),
        currentBucket: data.currentBucket
      };
    }
  }

  currentBucket(json, state) {
    let data = _.get(json, 'setCurrentBucket', false);
    if (data) {
      state.s3.configuration.currentBucket = data;
    }
  }

  addBucket(json, state) {
    let data = _.get(json, 'addBucket', false);
    if (data) {
      state.s3.configuration.buckets =
        state.s3.configuration.buckets.add(data);
    }
  }

  removeBucket(json, state) {
    let data = _.get(json, 'removeBucket', false);
    if (data) {
      state.s3.configuration.buckets =
        state.s3.configuration.buckets.delete(data);
    }
  }

  endpoint(json, state) {
    let data = _.get(json, 'setEndpoint', false);
    if (data) {
      state.s3.credentials.endpoint = data;
    }
  }

  accessKeyId(json, state) {
    let data = _.get(json, 'setAccessKeyId', false);
    if (data) {
      state.s3.credentials.accessKeyId = data;
    }
  }

  secretAccessKey(json, state) {
    let data = _.get(json, 'setSecretAccessKey', false);
    if (data) {
      state.s3.credentials.secretAccessKey = data;
    }
  }

}

