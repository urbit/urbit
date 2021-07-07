import { Tile, WeatherState } from '~/types/launch-update';
import {
  createState,
  createSubscription,
  reduceStateN
} from './base';
import airlock from '~/logic/api';
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
  baseHash: string | null;
  runtimeLag: boolean;
  getRuntimeLag: () => Promise<void>;
  getBaseHash: () => Promise<void>;
}

// @ts-ignore investigate zustand types
const useLaunchState = createState<LaunchState>(
  'Launch',
  (set, get) => ({
    firstTime: true,
    tileOrdering: [],
    tiles: {},
    weather: null,
    userLocation: null,
    baseHash: null,
    runtimeLag: false,
    getBaseHash: async () => {
      const baseHash = await airlock.scry({
        app: 'file-server',
        path: '/clay/base/hash'
      });
      set({ baseHash });
    },
    getRuntimeLag: async () => {
      const runtimeLag = await airlock.scry({
        app: 'launch',
        path: '/runtime-lag'
      });
      set({ runtimeLag });
    }
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
