// Very simple GCP Storage client.
//
import querystring from 'querystring';

export interface UploadParams {
  bucket: string;               // the bucket to upload the object to
  name: string;                 // the desired location within the bucket
  type: string;                 // the object's mime-type
  body: File;                   // the object itself
  predefinedAcl?: string;       // optional acl, usually you want 'publicRead'
}


const ENDPOINT = 'storage.googleapis.com';
const BASE_URL = 'https://storage.googleapis.com/upload/storage/v1/b';

export default class GcpClient {
  #accessKey: string;

  constructor(accessKey: string) {
    this.#accessKey = accessKey;
  }

  async upload({bucket, name, type, body, predefinedAcl}: UploadParams) {
    const urlParams = {
      uploadType: 'media',
      name,
      predefinedAcl
    };
    const url = `https://${ENDPOINT}/upload/storage/v1/b/${bucket}/o?` +
      querystring.stringify(urlParams);
    const headers = new Headers();
    headers.append('Authorization', `Bearer ${this.#accessKey}`);
    headers.append('Content-Type', type);
    const response = await fetch(url, {
      method: 'POST',
      mode: 'cors',
      cache: 'default',
      headers,
      referrerPolicy: 'no-referrer',
      body
    });
    // TODO: response errors: 400 -> perhaps need fine-grained permissions on
    return `https://${ENDPOINT}/${bucket}/${name}`;
  }
}
