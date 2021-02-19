export type Key = string;
export type Value = string | boolean | number;
export type Bucket = Map<string, Value>;
export type Settings = Map<string, Bucket>;

interface PutBucket {
  "put-bucket": {
    "bucket-key": Key;
    "bucket": Bucket;
  };
}

interface DelBucket {
  "del-bucket": {
    "bucket-key": Key;
  };
}

interface PutEntry {
  "put-entry": {
    "bucket-key": Key;
    "entry-key": Key;
    "value": Value;
  };
}

interface DelEntry {
  "del-entry": {
    "bucket-key": Key;
    "entry-key": Key;
  };
}

interface AllData {
  "all": Settings;
}

interface BucketData {
  "bucket": Bucket;
}

interface EntryData {
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
