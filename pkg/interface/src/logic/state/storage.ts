import { BaseState, createState } from "./base";

export interface GcpToken {
  accessKey: string;
  expiresIn: number;
}

export interface StorageState extends BaseState<StorageState> {
  gcp: {
    configured?: boolean;
    token?: GcpToken;
  },
  s3: {
    configuration: {
      buckets: Set<string>;
      currentBucket: string;
    };
    credentials: any | null; // TODO better type
  }
};

const useStorageState = createState<StorageState>('Storage', {
  gcp: {},
  s3: {
    configuration: {
      buckets: new Set(),
      currentBucket: ''
    },
    credentials: null,
  }
}, ['s3']);

export default useStorageState;