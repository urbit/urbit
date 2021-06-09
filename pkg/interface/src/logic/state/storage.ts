import { reduce } from '../reducers/s3-update';
import _ from 'lodash';
import { createState, createSubscription, reduceStateN } from './base';

export interface GcpToken {
  accessKey: string;
  expiresIn: number;
}

export interface StorageState {
  gcp: {
    configured?: boolean;
    token?: GcpToken;
  };
  s3: {
    configuration: {
      buckets: Set<string>;
      currentBucket: string;
    };
    credentials: any | null; // TODO better type
  };
}

// @ts-ignore investigate zustand types
const useStorageState = createState<StorageState>(
  'Storage',
  {
    gcp: {},
    s3: {
      configuration: {
        buckets: new Set(),
        currentBucket: ''
      },
      credentials: null
    }
  },
  ['s3', 'gcp'],
  [
    (set, get) =>
      createSubscription('s3-store', '/all', (e) => {
        const d = _.get(e, 's3-update', false);
        if (d) {
          reduceStateN(get(), d, reduce);
        }
      })
  ]
);

export default useStorageState;
