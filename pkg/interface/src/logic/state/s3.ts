import { BaseState, createState } from "./base";

export interface S3State extends BaseState<S3State> {
  configuration: {
    buckets: Set<string>;
    currentBucket: string;
  };
  credentials: any | null; // TODO better type
};

const useS3State = createState<S3State>('S3', {
  configuration: {
    buckets: new Set(),
    currentBucket: ''
  },
  credentials: null,
}, ['configuration']);

export default useS3State;