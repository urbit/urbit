import _ from 'lodash';
import { LaunchState, LaunchUpdate, WeatherState } from '~/types/launch-update';
import { Cage } from '~/types/cage';
import useLaunchState from '../state/launch';
import { compose } from 'lodash/fp';

export default class LaunchReducer {
  reduce(json: Cage) {
    const data = _.get(json, 'launch-update', false);
    if (data) {
      useLaunchState.setState(
        compose([
          initial,
          changeFirstTime,
          changeOrder,
          changeFirstTime,
          changeIsShown,
        ].map(reducer => reducer.bind(reducer, data))
        )(useLaunchState.getState())
      )
    }

    const weatherData: WeatherState = _.get(json, 'weather', false);
    if (weatherData) {
      useLaunchState.setState({ weather: weatherData });
    }

    const locationData = _.get(json, 'location', false);
    if (locationData) {
      useLaunchState.setState({ userLocation: locationData });
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