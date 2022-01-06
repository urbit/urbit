import { PatpNoSig } from '@urbit/api';
import useHarkState from '~/logic/state/hark';

declare global {
  interface Window {
    ship: PatpNoSig;
    desk: string;
    hark: typeof useHarkState.getState;
  }
}
