import { PatpNoSig } from '@urbit/api';
import useHarkState from '~/logic/state/hark';

declare global {
  interface Window {
    ship: PatpNoSig;
    hark: typeof useHarkState.getState;
  }
}
