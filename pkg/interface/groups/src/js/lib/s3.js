export default class S3Client {
  constructor() {
    this.s3 = null;
    this.AWS = window.AWS;    

    this.endpoint = new this.AWS.Endpoint("");
    this.accessKeyId = "";
    this.secretAccesskey = "";
  }

  setCredentials(endpoint, accessKeyId, secretAccessKey) {
    this.AWS = window.AWS;
    
    this.endpoint = new this.AWS.Endpoint(endpoint);
    this.accessKeyId = accessKeyId;
    this.secretAccessKey = secretAccessKey;

    this.s3 = 
      new this.AWS.S3({
        endpoint: this.endpoint,
        credentials: new this.AWS.Credentials({
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

