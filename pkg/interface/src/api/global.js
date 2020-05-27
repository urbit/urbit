import BaseApi from './base';

class PrivateHelper extends BaseApi {
  setSelected(selected) {
    this.store.handleEvent({
      data: {
        local: {
          selected: selected
        }
      }
    });
  }

}

export default class GlobalApi {
  constructor(ship, channel, store) {
    const helper = new PrivateHelper(ship, channel, store);

    this.ship = ship;
    this.subscribe = helper.subscribe.bind(helper);

    this.setSelected = helper.setSelected.bind(helper);
  }
}

