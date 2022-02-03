import { Poke, Scry } from '../lib';
import { PutBucket, Key, Bucket, DelBucket, Value, PutEntry, DelEntry, SettingsUpdate } from './types';
export declare const action: <T extends SettingsUpdate>(data: T) => Poke<T>;
export declare const putBucket: (desk: string, key: Key, bucket: Bucket) => Poke<PutBucket>;
export declare const delBucket: (desk: string, key: Key) => Poke<DelBucket>;
export declare const putEntry: (desk: string, bucket: Key, key: Key, value: Value) => Poke<PutEntry>;
export declare const delEntry: (desk: string, bucket: Key, key: Key) => Poke<DelEntry>;
export declare const getAll: Scry;
export declare const getBucket: (desk: string, bucket: string) => {
    app: string;
    path: string;
};
export declare const getEntry: (desk: string, bucket: string, entry: string) => {
    app: string;
    path: string;
};
export declare const getDeskSettings: (desk: string) => {
    app: string;
    path: string;
};
export * from './types';
