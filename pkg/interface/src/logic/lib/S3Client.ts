import { StorageClient, StorageUpload, UploadParams } from './StorageClient';
import type S3 from 'aws-sdk/clients/s3';

export default class S3Client implements StorageClient {
  config: S3.ClientConfiguration;
  client: S3 | null = null;
  S3: typeof import('aws-sdk/clients/s3');

  constructor(config: S3.ClientConfiguration) {
    this.config = config;
  }

  async initAndUpload(params: UploadParams) {
    if (!this.S3) {
      await this.loadLib();
    }

    if (!this.client) {
      this.client = new this.S3(this.config);
    }

    return this.client.upload(params).promise();
  }

  upload(params: UploadParams): StorageUpload {
    const upload = this.initAndUpload.bind(this);
    return {
      promise: () => upload(params)
    };
  }

  async loadLib() {
    this.S3 = (await import('aws-sdk/clients/s3')).default;
  }
}
