import { useLeapStore } from './nav/Nav';
import { useRecentsStore } from './nav/search/Home';
import useDocketState from './state/docket';

declare global {
  interface Window {
    ship: string;
    desk: string;
    recents: typeof useRecentsStore.getState;
    docket: typeof useDocketState.getState;
    leap: typeof useLeapStore.getState;
  }
}

export {};
