import S3 from 'aws-sdk/clients/s3';

export default class S3Client {
  constructor() {
    this.s3 = null;

    this.endpoint = '';
    this.accessKeyId = '';
    this.secretAccesskey = '';
  }

  setCredentials(endpoint, accessKeyId, secretAccessKey) {
    this.endpoint = endpoint;
    this.accessKeyId = accessKeyId;
    this.secretAccessKey = secretAccessKey;

    this.s3 = new S3({
      endpoint: endpoint,
      credentials: {
        accessKeyId: this.accessKeyId,
        secretAccessKey: this.secretAccessKey
      }
    });
  }

  async upload(bucket, filename, buffer) {
    const params = {
      Bucket: bucket,
      Key:  filename,
      Body: buffer,
      ACL: 'public-read',
      ContentType: buffer.type
    };

    if(!this.s3) {
      throw new Error('S3 not initialized');
    }
    return this.s3.upload(params).promise();
  }
}

