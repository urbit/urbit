import { InviteReducer } from '/reducers/invite.js';
import { MetadataReducer } from '/reducers/metadata.js';

class Store {
  constructor() {
    this.state = {
      invites: {},
      associations: {
        contacts: {}
      }
    };
    this.setState = () => {};
    this.inviteReducer = new InviteReducer();
    this.metadataReducer = new MetadataReducer();
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    if (("from" in data) && ((data.from.app === "invite-view") || data.from.app === "metadata-store")) {
      this.inviteReducer.reduce(data.data, this.state);
      this.metadataReducer.reduce(data.data, this.state);
    }
    else {
    let json = data.data;
    this.setState(json);
    }
    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
