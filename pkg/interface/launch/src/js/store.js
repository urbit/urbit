import { InviteReducer } from '/reducers/invite.js';

class Store {
  constructor() {
    this.state = {
      invites: {}
    };
    this.setState = () => {};
    this.inviteReducer = new InviteReducer();
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    if (("from" in data) && data.from.app === "invite-view") {
      this.inviteReducer.reduce(data.data, this.state)
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
