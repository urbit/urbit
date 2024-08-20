import { Tile, WeatherState } from '~/types/launch-update';
import {
  createState,
  createSubscription,
  reduceStateN
} from './base';
import { reduce } from '../reducers/launch-update';
import _ from 'lodash';

export interface LaunchState {
  firstTime: boolean;
  tileOrdering: string[];
  tiles: {
    [app: string]: Tile;
  };
  weather: WeatherState | null | Record<string, never> | boolean;
  userLocation: string | null;
}

// @ts-ignore investigate zustand types
const useLaunchState = createState<LaunchState>(
  'Launch',
  (set, get) => ({
    firstTime: true,
    tileOrdering: [],
    tiles: {},
    weather: null,
    userLocation: null
  }),
  ['weather'],
  [
    (set, get) =>
      createSubscription('weather', '/all', (e) => {
        const w = _.get(e, 'weather', false);
        if (w) {
          set({ weather: w });
        }
        const l = _.get(e, 'location', false);
        if (l) {
          set({ userLocation: l });
        }
      }),
    (set, get) =>
      createSubscription('launch', '/all', (e) => {
        const d = _.get(e, 'launch-update', false);
        if (d) {
          reduceStateN(get(), d, reduce);
        }
      })
  ]
);

export default useLaunchState;
