import BaseApi from './base';
import { StoreState } from '../store/type';
import {S3Update} from '../../types/s3-update';


export default class S3Api extends BaseApi<StoreState> {

  setCurrentBucket(bucket: string) {
    this.s3Action({ 'set-current-bucket': bucket });
  }

  addBucket(bucket: string) {
    this.s3Action({ 'add-bucket': bucket });
  }

  removeBucket(bucket: string) {
    this.s3Action({ 'remove-bucket': bucket });
  }

  setEndpoint(endpoint: string) {
    this.s3Action({ 'set-endpoint': endpoint });
  }

  setAccessKeyId(accessKeyId: string) {
    this.s3Action({ 'set-access-key-id': accessKeyId });
  }

  setSecretAccessKey(secretAccessKey: string) {
    this.s3Action({ 'set-secret-access-key': secretAccessKey });
  }

  private s3Action(data: any) {
    this.action('s3-store', 's3-action', data);
  }

}

