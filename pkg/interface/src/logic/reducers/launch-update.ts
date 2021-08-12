import _ from 'lodash';
import { LaunchUpdate } from '~/types/launch-update';
import { LaunchState as State } from '../state/launch';
import { BaseState } from '../state/base';

type LaunchState = State & BaseState<State>;

export const initial = (json: LaunchUpdate, state: LaunchState): LaunchState => {
  const data = _.get(json, 'initial', false);
  if (data) {
    Object.keys(data).forEach((key) => {
      state[key] = data[key];
    });
  }
  return state;
};

export const changeFirstTime = (json: LaunchUpdate, state: LaunchState): LaunchState => {
  const data = _.get(json, 'changeFirstTime', false);
  if (data) {
    state.firstTime = data;
  }
  return state;
};

export const changeOrder = (json: LaunchUpdate, state: LaunchState): LaunchState => {
  const data = _.get(json, 'changeOrder', false);
  if (data) {
    state.tileOrdering = data;
  }
  return state;
};

export const changeIsShown = (json: LaunchUpdate, state: LaunchState): LaunchState => {
  const data = _.get(json, 'changeIsShown', false);
  if (data) {
    const tile = state.tiles[data.name];
    if (tile) {
      tile.isShown = data.isShown;
    }
  }
  return state;
};

export const reduce = [
  initial,
  changeFirstTime,
  changeOrder,
  changeFirstTime,
  changeIsShown
];
