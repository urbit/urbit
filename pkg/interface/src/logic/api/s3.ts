import BaseApi from './base';
import { StoreState } from '../store/type';
import { S3Update } from '../../types/s3-update';

export default class S3Api extends BaseApi<StoreState> {
  setCurrentBucket(bucket: string) {
    return this.s3Action({ 'set-current-bucket': bucket });
  }

  addBucket(bucket: string) {
    return this.s3Action({ 'add-bucket': bucket });
  }

  removeBucket(bucket: string) {
    return this.s3Action({ 'remove-bucket': bucket });
  }

  setEndpoint(endpoint: string) {
    return this.s3Action({ 'set-endpoint': endpoint });
  }

  setAccessKeyId(accessKeyId: string) {
    return this.s3Action({ 'set-access-key-id': accessKeyId });
  }

  setSecretAccessKey(secretAccessKey: string) {
    return this.s3Action({ 'set-secret-access-key': secretAccessKey });
  }

  private s3Action(data: any) {
    return this.action('s3-store', 's3-action', data);
  }
}

