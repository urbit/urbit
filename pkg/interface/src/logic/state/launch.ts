import { Tile, WeatherState } from "~/types/launch-update";

import { BaseState, createState } from "./base";


export interface LaunchState extends BaseState<LaunchState> {
  firstTime: boolean;
  tileOrdering: string[];
  tiles: {
    [app: string]: Tile;
  },
  weather: WeatherState | null,
  userLocation: string | null;
  baseHash: string | null;
};

const useLaunchState = createState<LaunchState>('Launch', {
  firstTime: true,
  tileOrdering: [],
  tiles: {},
  weather: null,
  userLocation: null,
  baseHash: null
});


export default useLaunchState;