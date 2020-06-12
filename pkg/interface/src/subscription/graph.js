import BaseSubscription from './base';

let getRandomInt = (max) => {
  return Math.floor(Math.random() * Math.floor(max));
}

export default class GraphSubscription extends BaseSubscription {
  constructor(store, api, channel) {
    super(store, api, channel);
    this.connectionNumber = getRandomInt(999);
  }

  start() {
    this.subscribe('/updates/' + this.connectionNumber, 'graph-view');
  }

  handleEvent(diff) {
    if ('graph-view' in diff) {
      this.api.fetch(connectionNumber); 
    } else {
      // extend
      this.store.handleEvent(diff);
    }
  }
}

