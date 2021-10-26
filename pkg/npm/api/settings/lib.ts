import { Poke, Scry } from "../lib";
import { PutBucket, Key, Bucket, DelBucket, Value, PutEntry, DelEntry, SettingsUpdate } from './types';

export const action = <T extends SettingsUpdate>(data: T): Poke<T> => ({
  app: 'settings-store',
  mark: 'settings-event',
  json: data
});

export const putBucket = (
  key: Key,
  bucket: Bucket
): Poke<PutBucket> => action({
  'put-bucket': {
    'bucket-key': key,
    'bucket': bucket
  }
});

export const delBucket = (
  key: Key
): Poke<DelBucket> => action({
  'del-bucket': {
    'bucket-key': key
  }
});

export const putEntry = (
  bucket: Key,
  key: Key,
  value: Value
): Poke<PutEntry> => action({
  'put-entry': {
    'bucket-key': bucket,
    'entry-key': key,
    value: value
  }
});

export const delEntry = (
  bucket: Key,
  key: Key
): Poke<DelEntry> => action({
  'del-entry': {
    'bucket-key': bucket,
    'entry-key': key
  }
});

export const getAll: Scry = {
  app: 'settings-store',
  path: '/all'
}

export const getBucket = (bucket: string) => ({
  app: 'settings-store',
  path: `/bucket/${bucket}`
});

export const getEntry = (bucket: string, entry: string) => ({
  app: 'settings-store',
  path: `/entry/${bucket}/${entry}`
});
    

export * from './types';
