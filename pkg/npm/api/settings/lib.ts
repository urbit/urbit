import { Poke, Scry } from '../lib';
import { PutBucket, Key, Bucket, DelBucket, Value, PutEntry, DelEntry, SettingsUpdate } from './types';

export const action = <T extends SettingsUpdate>(data: T): Poke<T> => ({
  app: 'settings-store',
  mark: 'settings-event',
  json: data
});

export const putBucket = (
  desk: string,
  key: Key,
  bucket: Bucket
): Poke<PutBucket> => action({
  'put-bucket': {
    desk,
    'bucket-key': key,
    'bucket': bucket
  }
});

export const delBucket = (
  desk: string,
  key: Key
): Poke<DelBucket> => action({
  'del-bucket': {
    desk,
    'bucket-key': key
  }
});

export const putEntry = (
  desk: string,
  bucket: Key,
  key: Key,
  value: Value
): Poke<PutEntry> => action({
  'put-entry': {
    desk,
    'bucket-key': bucket,
    'entry-key': key,
    value: value
  }
});

export const delEntry = (
  desk: string,
  bucket: Key,
  key: Key
): Poke<DelEntry> => action({
  'del-entry': {
    desk,
    'bucket-key': bucket,
    'entry-key': key
  }
});

export const getAll: Scry = {
  app: 'settings-store',
  path: '/all'
};

export const getBucket = (desk: string, bucket: string) => ({
  app: 'settings-store',
  path: `/bucket/${bucket}`
});

export const getEntry = (desk: string, bucket: string, entry: string) => ({
  app: 'settings-store',
  path: `/entry/${desk}/${bucket}/${entry}`
});

export * from './types';
