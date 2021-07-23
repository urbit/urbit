// Very simple GCP Storage client.
//
// It's designed to match a subset of the S3 client upload API. The upload
// function on S3 returns a ManagedUpload, which has a promise() method on
// it. We don't care about any of the other methods on ManagedUpload, so we
// just do the work in its promise() method.
//
import querystring from 'querystring';
import {
  StorageAcl,
  StorageClient,
  StorageUpload,
  UploadParams,
  UploadResult
} from './StorageClient';


const ENDPOINT = 'storage.googleapis.com';

class GcpUpload implements StorageUpload {
  #params: UploadParams;
  #accessKey: string;

  constructor(params: UploadParams, accessKey: string) {
    this.#params = params;
    this.#accessKey = accessKey;
  }

  async promise(): Promise<UploadResult> {
    const {Bucket, Key, ContentType, Body} = this.#params;
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
    if (!response.ok) {
      console.error('GcpClient server error', await response.json());
      throw new Error(`GcpClient: response ${response.status}`);
    }
    return {Location: `https://${ENDPOINT}/${Bucket}/${Key}`};
  }
}

export default class GcpClient implements StorageClient {
  #accessKey: string;

  constructor(accessKey: string) {
    this.#accessKey = accessKey;
  }

  upload(params: UploadParams): StorageUpload {
    return new GcpUpload(params, this.#accessKey);
  }
}
