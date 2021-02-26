// Very simple GCP Storage client.
//
import querystring from 'querystring';
import {
  StorageAcl,
  StorageClient,
  UploadParams,
  UploadResult
} from './StorageClient';


const ENDPOINT = 'storage.googleapis.com';

export default class GcpClient implements StorageClient {
  #accessKey: string;

  constructor(accessKey: string) {
    this.#accessKey = accessKey;
  }

  async upload({Bucket, Key, ContentType, Body}: UploadParams): UploadResult {
    const urlParams = {
      uploadType: 'media',
      name: Key,
      predefinedAcl: 'publicRead'
    };
    const url = `https://${ENDPOINT}/upload/storage/v1/b/${Bucket}/o?` +
      querystring.stringify(urlParams);
    const headers = new Headers();
    headers.append('Authorization', `Bearer ${this.#accessKey}`);
    headers.append('Content-Type', ContentType);
    const response = await fetch(url, {
      method: 'POST',
      mode: 'cors',
      cache: 'default',
      headers,
      referrerPolicy: 'no-referrer',
      body: Body
    });
    // TODO: response errors.
    // If we get a 400, perhaps we need fine-grained permissions on the bucket.
    return {Location: `https://${ENDPOINT}/${Bucket}/${Key}`};
  }
}
