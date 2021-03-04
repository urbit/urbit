import { Poke } from '../lib/types';
import { S3Update, S3UpdateAccessKeyId, S3UpdateAddBucket, S3UpdateCurrentBucket, S3UpdateEndpoint, S3UpdateRemoveBucket, S3UpdateSecretAccessKey } from './types';

const s3Action = <T extends S3Update>(
  data: any
): Poke<T> => ({
  app: 's3-store',
  mark: 's3-action',
  json: data
});

export const setCurrentBucket = (
  bucket: string
): Poke<S3UpdateCurrentBucket> => s3Action({
  'set-current-bucket': bucket
});

export const addBucket = (
  bucket: string
): Poke<S3UpdateAddBucket> => s3Action({
  'add-bucket': bucket
});

export const removeBucket = (
  bucket: string
): Poke<S3UpdateRemoveBucket> => s3Action({
  'remove-bucket': bucket
});

export const setEndpoint = (
  endpoint: string
): Poke<S3UpdateEndpoint> => s3Action({
  'set-endpoint': endpoint
});

export const setAccessKeyId = (
  accessKeyId: string
): Poke<S3UpdateAccessKeyId> => s3Action({
  'set-access-key-id': accessKeyId
});

export const setSecretAccessKey = (
  secretAccessKey: string
): Poke<S3UpdateSecretAccessKey> => s3Action({
  'set-secret-access-key': secretAccessKey
});

