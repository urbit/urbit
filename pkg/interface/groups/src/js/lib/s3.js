const AWS = require('aws-sdk')

class S3 {
  constructor() {
    this.endpoint = new AWS.Endpoint("");
    this.accessKeyId = "";
    this.secretAccesskey = "";

    this.s3 = null;
  }

  setCredentials(endpoint, accessKeyId, secretAccessKey) {
    this.endpoint = new AWS.Endpoint(endpoint);
    this.accessKeyId = accessKeyId;
    this.secretAccessKey = secretAccessKey;

    this.s3 = 
      new AWS.S3({
        endpoint: this.endpoint,
        credentials: new AWS.Credentials({
          accessKeyId: this.accessKeyId,
          secretAccessKey: this.secretAccessKey
        })
      });
  }

  createBucket(name) {
    let params = {
      Bucket: name,
      ACL: "public-read"
    };

    return new Promise((resolve, reject) => {
      if (!this.s3) {
        reject({ error: 'S3 not initialized!' });
        return;
      }
      this.s3.createBucket(params, (error, data) => {
        if (error) {
          reject({ error });
        } else {
          resolve({ data });
        }
      });
    });
  }

  listBuckets() {
    return new Promise((resolve, reject) => {
      if (!this.s3) {
        reject({ error: 'S3 not initialized!' });
        return;
      }
      this.s3.listBuckets({}, (error, data) => {
        if (error) {
          reject({ error });
        } else {
          resolve({ data });
        }
      });
    });
  }

  upload(bucket, filename, buffer) {
    let params = {
      Bucket: bucket,
      Key:  filename,
      Body: buffer
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
          resolve({ data });
        }
      });
    });
  }
}

export let s3 = new S3();

