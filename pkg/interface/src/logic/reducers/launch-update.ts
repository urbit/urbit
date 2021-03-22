import _ from 'lodash';
import { LaunchUpdate } from '~/types/launch-update';
import { Cage } from '~/types/cage';
import { StoreState } from '../../store/type';

type LaunchState = Pick<StoreState, 'launch' | 'weather' | 'userLocation'>;

export default class LaunchReducer<S extends LaunchState> {
  reduce(json: Cage, state: S) {
    const data = _.get(json, 'launch-update', false);
    if (data) {
      this.initial(data, state);
      this.changeFirstTime(data, state);
      this.changeOrder(data, state);
      this.changeFirstTime(data, state);
      this.changeIsShown(data, state);
    }

    const weatherData = _.get(json, 'weather', false);
    if (weatherData) {
      state.weather = weatherData;
    }

    const locationData = _.get(json, 'location', false);
    if (locationData) {
      state.userLocation = locationData;
    }
  }

  initial(json: LaunchUpdate, state: S) {
    const data = _.get(json, 'initial', false);
    if (data) {
      state.launch = data;
    }
  }

  changeFirstTime(json: LaunchUpdate, state: S) {
    const data = _.get(json, 'changeFirstTime', false);
    if (data) {
      state.launch.firstTime = data;
    }
  }

  changeOrder(json: LaunchUpdate, state: S) {
    const data = _.get(json, 'changeOrder', false);
    if (data) {
      state.launch.tileOrdering = data;
    }
  }

  changeIsShown(json: LaunchUpdate, state: S) {
    const data = _.get(json, 'changeIsShown', false);
    if (data) {
      const tile = state.launch.tiles[data.name];
      console.log(tile);
      if (tile) {
        tile.isShown = data.isShown;
      }
    }
  }
}
