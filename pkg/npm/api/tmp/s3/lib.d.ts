import { Poke } from '../lib/types';
import { S3UpdateAccessKeyId, S3UpdateAddBucket, S3UpdateCurrentBucket, S3UpdateEndpoint, S3UpdateRemoveBucket, S3UpdateSecretAccessKey } from './types';
export declare const setCurrentBucket: (bucket: string) => Poke<S3UpdateCurrentBucket>;
export declare const addBucket: (bucket: string) => Poke<S3UpdateAddBucket>;
export declare const removeBucket: (bucket: string) => Poke<S3UpdateRemoveBucket>;
export declare const setEndpoint: (endpoint: string) => Poke<S3UpdateEndpoint>;
export declare const setAccessKeyId: (accessKeyId: string) => Poke<S3UpdateAccessKeyId>;
export declare const setSecretAccessKey: (secretAccessKey: string) => Poke<S3UpdateSecretAccessKey>;
