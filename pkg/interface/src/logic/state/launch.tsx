import React from "react";
import create, { State } from "zustand";
import { persist } from "zustand/middleware";
import { Tile, WeatherState } from "~/types/launch-update";
import { stateSetter } from "../lib/util";


export interface LaunchState extends State {
  firstTime: boolean;
  tileOrdering: string[];
  tiles: {
    [app: string]: Tile;
  },
  weather: WeatherState | null,
  userLocation: string | null;
  set: (fn: (state: LaunchState) => void) => void;
};

const useLaunchState = create<LaunchState>(persist((set, get) => ({
  firstTime: true,
  tileOrdering: [],
  tiles: {},
  weather: null,
  userLocation: null,
  set: fn => stateSetter(fn, set)
}), {
  name: 'LandscapeLaunchState'
}));

function withLaunchState<P, S extends keyof LaunchState>(Component: any, stateMemberKeys?: S[]) {
  return React.forwardRef((props: Omit<P, S>, ref) => {
    const launchState = stateMemberKeys ? useLaunchState(
      state => stateMemberKeys.reduce(
        (object, key) => ({ ...object, [key]: state[key] }), {}
      )
    ): useLaunchState();
    return <Component ref={ref} {...launchState} {...props} />
  });
}

export { useLaunchState as default, withLaunchState };