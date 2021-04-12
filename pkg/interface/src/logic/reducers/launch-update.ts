import _ from 'lodash';
import { LaunchUpdate, WeatherState } from '~/types/launch-update';
import { Cage } from '~/types/cage';
import useLaunchState, { LaunchState } from '../state/launch';
import { compose } from 'lodash/fp';
import { reduceState } from '../state/base';

export default class LaunchReducer {
  reduce(json: Cage) {
    const data = _.get(json, 'launch-update', false);
    if (data) {
      reduceState<LaunchState, LaunchUpdate>(useLaunchState, data, [
        initial,
        changeFirstTime,
        changeOrder,
        changeFirstTime,
        changeIsShown,
      ]);
    }

    const weatherData: WeatherState = _.get(json, 'weather', false);
    if (weatherData) {
      useLaunchState.getState().set(state => {
        state.weather = weatherData;
      });
    }

    const locationData = _.get(json, 'location', false);
    if (locationData) {
      useLaunchState.getState().set(state => {
        state.userLocation = locationData;
      });
    }

    const baseHash = _.get(json, 'baseHash', false);
    if (baseHash) {
      useLaunchState.getState().set(state => {
        state.baseHash = baseHash;
      })
    }
  }
}

export const initial = (json: LaunchUpdate, state: LaunchState): LaunchState => {
  const data = _.get(json, 'initial', false);
  if (data) {
    Object.keys(data).forEach(key => {
      state[key] = data[key];
    });
  }
  return state;
}

export const changeFirstTime = (json: LaunchUpdate, state: LaunchState): LaunchState => {
  const data = _.get(json, 'changeFirstTime', false);
  if (data) {
    state.firstTime = data;
  }
  return state;
}

export const changeOrder = (json: LaunchUpdate, state: LaunchState): LaunchState => {
  const data = _.get(json, 'changeOrder', false);
  if (data) {
    state.tileOrdering = data;
  }
  return state;
}

export const changeIsShown = (json: LaunchUpdate, state: LaunchState): LaunchState => {
  const data = _.get(json, 'changeIsShown', false);
  if (data) {
    const tile = state.tiles[data.name];
    if (tile) {
      tile.isShown = data.isShown;
    }
  }
  return state;
}