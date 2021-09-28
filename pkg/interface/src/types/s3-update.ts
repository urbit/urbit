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

interface S3UpdateCredentials {
  credentials: S3Credentials;
}

interface S3UpdateConfiguration {
  configuration: {
    buckets: string[];
    currentBucket: string;
  }
}

interface S3UpdateCurrentBucket {
  setCurrentBucket: string;
}

interface S3UpdateAddBucket {
  addBucket: string;
}

interface S3UpdateRemoveBucket {
  removeBucket: string;
}

interface S3UpdateEndpoint {
  setEndpoint: string;
}

interface S3UpdateAccessKeyId {
  setAccessKeyId: string;
}

interface S3UpdateSecretAccessKey {
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
