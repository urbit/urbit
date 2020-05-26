import _ from 'lodash';

export default class LaunchReducer {
  reduce(json, state) {
    const data = _.get(json, 'launch-update', false);
    if (data) {
      this.initial(data, state);
      this.changeFirstTime(data, state);
      this.changeOrder(data, state);
      this.changeFirstTime(data, state);
      this.changeIsShown(data, state);
    }
  }

  initial(json, state) {
    const data = _.get(json, 'initial', false);
    if (data) {
      state.launch = data;
    }
  }

  changeFirstTime(json, state) {
    const data = _.get(json, 'changeFirstTime', false);
    if (data) {
      state.launch.firstTime = data;
    }
  }

  changeOrder(json, state) {
    const data = _.get(json, 'changeOrder', false);
    if (data) {
      state.launch.tileOrdering = data;
    }
  }

  changeIsShown(json, state) {
    const data = _.get(json, 'changeIsShown', false);
    console.log(json, data);
    if (data) {
      let tile = state.launch.tiles[data.name];
      console.log(tile);
      if (tile) {
        tile.isShown = data.isShown;
      }
    }
  }

}
