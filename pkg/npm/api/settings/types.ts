export type Key = string;
export type Value = string | string[] | boolean | number;
export type Bucket = { [key: string]: Value; };
export type DeskSettings = { [bucket: string]: Bucket; };
export type Settings = { [desk: string]: Settings; }

export interface PutBucket {
  'put-bucket': {
    desk: string;
    'bucket-key': Key;
    'bucket': Bucket;
  };
}

export interface DelBucket {
  'del-bucket': {
    desk: string;
    'bucket-key': Key;
  };
}

export interface PutEntry {
  'put-entry': {
    'bucket-key': Key;
    'entry-key': Key;
    'value'?: Value;
  };
}

export interface DelEntry {
  'del-entry': {
    desk: string;
    'bucket-key': Key;
    'entry-key': Key;
  };
}

export interface AllData {
  'all': Settings;
}

export interface DeskData {
  desk: DeskSettings;
}

export interface BucketData {
  'bucket': Bucket;
}

export interface EntryData {
  'entry': Value;
}

export type SettingsUpdate =
  | PutBucket
  | DelBucket
  | PutEntry
  | DelEntry;

export type SettingsData =
  | AllData
  | BucketData
  | EntryData
  | DeskData;
