// Defines a StorageClient interface interoperable between S3 and GCP Storage.
//


// XX kind of gross. S3 needs 'public-read', GCP needs 'publicRead'.
// Rather than write a wrapper around S3, we offer this field here, which
// should always be passed, and will be replaced by 'publicRead' in the
// GCP client.
export enum StorageAcl {
  PublicRead = 'public-read'
};

export interface UploadParams {
  Bucket: string;               // the bucket to upload the object to
  Key: string;                  // the desired location within the bucket
  ContentType: string;          // the object's mime-type
  ACL: StorageAcl;              // ACL, always 'public-read'
  Body: File;                   // the object itself
};

export interface UploadResult {
  Location: string;
};

// Extra layer of indirection used by S3 client.
export interface StorageUpload {
  promise(): Promise<UploadResult>;
};

export interface StorageClient {
  upload(params: UploadParams): StorageUpload;
};
