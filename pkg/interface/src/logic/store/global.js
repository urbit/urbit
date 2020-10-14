import BaseStore from './base';
import InviteReducer from '../reducers/invite-update';
import MetadataReducer from '../reducers/metadata-update';
import LocalReducer from '../reducers/local';


export default class GlobalStore extends BaseStore {
  constructor() {
    super();
    this.inviteReducer = new InviteReducer();
    this.metadataReducer = new MetadataReducer();
    this.localReducer = new LocalReducer();
  }

  initialState() {
    return {
      invites: {},
      associations: {},
      selectedGroups: []
    };
  }

  reduce(data, state) {
    this.inviteReducer.reduce(data, this.state);
    this.metadataReducer.reduce(data, this.state);
    this.localReducer.reduce(data, this.state);
  }
}

