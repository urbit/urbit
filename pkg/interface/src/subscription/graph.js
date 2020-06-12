import BaseSubscription from './base';

let getRandomInt = (max) => {
  return Math.floor(Math.random() * Math.floor(max));
}

export default class GraphSubscription extends BaseSubscription {
  constructor(store, api, channel) {
    super(store, api, channel);
    this.connectionNumber = getRandomInt(999);
    console.log(this.connectionNumber);
  }

  start() {
    this.subscribe('/updates/' + this.connectionNumber, 'graph-view');
  }

  handleEvent(diff) {
    if ('graph-view' in diff.data) {
      this.api.fetch(this.connectionNumber); 
    } else {
      // extend
      this.store.handleEvent(diff);
    }
  }
}

