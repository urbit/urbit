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
    this.launchAction({ changeOrder: orderedTiles });
  }

  launchChangeFirstTime(firstTime = true) {
    this.launchAction({ changeFirstTime: firstTime });
  }

  launchChangeIsShown(isShown = true) {
    this.launchAction({ isShown });
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
  }
}

