export type Key = string;
export type Value = string | string[] | boolean | number;
export type Bucket = Map<string, Value>;
export type Settings = Map<string, Bucket>;

export interface PutBucket {
  "put-bucket": {
    "bucket-key": Key;
    "bucket": Bucket;
  };
}

export interface DelBucket {
  "del-bucket": {
    "bucket-key": Key;
  };
}

export interface PutEntry {
  "put-entry": {
    "bucket-key": Key;
    "entry-key": Key;
    "value": Value;
  };
}

export interface DelEntry {
  "del-entry": {
    "bucket-key": Key;
    "entry-key": Key;
  };
}

export interface AllData {
  "all": Settings;
}

export interface BucketData {
  "bucket": Bucket;
}

export interface EntryData {
  "entry": Value;
}

export type SettingsUpdate =
  | PutBucket
  | DelBucket
  | PutEntry
  | DelEntry;

export type SettingsData =
  | AllData
  | BucketData
  | EntryData;
