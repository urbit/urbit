export interface S3Credentials {
  endpoint: string;
  accessKeyId: string;
  secretAccessKey: string;
}

export interface S3Configuration {
 buckets: Set<string>;
 currentBucket: string;
}

export interface S3State {
 configuration: S3Configuration;
 credentials: S3Credentials | null;
}

export interface S3UpdateCredentials {
 credentials: S3Credentials;
}

export interface S3UpdateConfiguration {
 configuration: {
   buckets: string[];
   currentBucket: string;
 }
}

export interface S3UpdateCurrentBucket {
 setCurrentBucket: string;
}

export interface S3UpdateAddBucket {
 addBucket: string;
}

export interface S3UpdateRemoveBucket {
 removeBucket: string;
}

export interface S3UpdateEndpoint {
 setEndpoint: string;
}

export interface S3UpdateAccessKeyId {
 setAccessKeyId: string;
}

export interface S3UpdateSecretAccessKey {
 setSecretAccessKey: string;
}

export type S3Update =
 S3UpdateCredentials
| S3UpdateConfiguration
| S3UpdateCurrentBucket
| S3UpdateAddBucket
| S3UpdateRemoveBucket
| S3UpdateEndpoint
| S3UpdateAccessKeyId
| S3UpdateSecretAccessKey;
