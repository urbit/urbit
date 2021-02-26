import create, { State } from "zustand";
import { persist } from "zustand/middleware";
import { stateSetter } from "../lib/util";

export interface S3State extends State {
  configuration: {
    buckets: Set<string>;
    currentBucket: string;
  };
  credentials: any | null; // TODO better type
  set: (fn: (state: S3State) => void) => void;
};

const useS3State = create<S3State>(persist((set, get) => ({
  configuration: {
    buckets: new Set(),
    currentBucket: ''
  },
  credentials: null,
  set: fn => stateSetter(fn, set)
}), {
  blacklist: ['configuration'],
  name: 'LandscapeS3State'
}));

export default useS3State;