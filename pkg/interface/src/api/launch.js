import BaseApi from './base';

class PrivateHelper extends BaseApi {
  launchAction(data) {
    this.action('launch', 'launch-action', data);
  }

  launchAdd(name, tile = { basic : { title: '', linkedUrl: '', iconUrl: '' }}) {
    this.launchAction({ add: { name, tile } });
  }

  launchRemove(name) {
    this.launchAction({ remove: name });
  }

  launchChangeOrder(orderedTiles = []) {
    this.launchAction({ 'change-order': orderedTiles });
  }

  launchChangeFirstTime(firstTime = true) {
    this.launchAction({ 'change-first-time': firstTime });
  }

  launchChangeIsShown(name, isShown = true) {
    this.launchAction({ 'change-is-shown': { name, isShown }});
  }

  clockAction(latlng) {
    return this.action('clock', 'json', latlng);
  }

  weatherAction(latlng) {
    return this.action('weather', 'json', latlng);
  }
}

export default class LaunchApi {
  constructor(ship, channel, store) {
    const helper = new PrivateHelper(ship, channel, store);

    this.ship = ship;
    this.subscribe = helper.subscribe.bind(helper);

    this.launch = {
      add: helper.launchAdd.bind(helper),
      remove: helper.launchRemove.bind(helper),
      changeOrder: helper.launchChangeOrder.bind(helper),
      changeFirstTime: helper.launchChangeFirstTime.bind(helper),
      changeIsShown: helper.launchChangeIsShown.bind(helper)
    };

    this.clock = { action: helper.clockAction.bind(helper) };

    this.weather ={ action: helper.weatherAction.bind(helper) };
  }
}

