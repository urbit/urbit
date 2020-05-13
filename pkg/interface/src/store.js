import InitialReducer from './reducers/initial';
import InviteUpdateReducer from './reducers/invite-update';
import MetadataReducer from './reducers/metadata-update';
import LocalReducer from './reducers/local';

export default class Store {
  constructor() {
    this.state = {
      invites: {},
      associations: {
        contacts: {}
      },
      selectedGroups: []
    };

    this.initialReducer = new InitialReducer();
    this.inviteUpdateReducer = new InviteUpdateReducer();
    this.metadataReducer = new MetadataReducer();
    this.localReducer = new LocalReducer();
    this.setState = () => {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    let json = data.data;

    this.initialReducer.reduce(json, this.state);
    this.inviteUpdateReducer.reduce(json, this.state);
    this.metadataReducer.reduce(json, this.state);
    this.localReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}
