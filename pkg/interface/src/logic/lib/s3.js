export default class S3Client {
  constructor() {
    this.s3 = null;

    this.endpoint = '';
    this.accessKeyId = '';
    this.secretAccesskey = '';
  }

  setCredentials(endpoint, accessKeyId, secretAccessKey) {
    if (!window.AWS) {
      setTimeout(() => {
        this.setCredentials(endpoint, accessKeyId, secretAccessKey);
      }, 2000);
      return;
    }
    this.endpoint = new window.AWS.Endpoint(endpoint);
    this.accessKeyId = accessKeyId;
    this.secretAccessKey = secretAccessKey;

    this.s3 =
      new window.AWS.S3({
        endpoint: this.endpoint,
        credentials: new window.AWS.Credentials({
          accessKeyId: this.accessKeyId,
          secretAccessKey: this.secretAccessKey
        })
      });
  }

  upload(bucket, filename, buffer) {
    const params = {
      Bucket: bucket,
      Key:  filename,
      Body: buffer,
      ACL: 'public-read',
      ContentType: buffer.type
    };
    return new Promise((resolve, reject) => {
      if (!this.s3) {
        reject({ error: 'S3 not initialized!' });
        return;
      }
      this.s3.upload(params, (error, data) => {
        if (error) {
          reject({ error });
        } else {
          resolve(data);
        }
      });
    });
  }
}

