import _ from 'lodash';


export class ContactUpdateReducer {
  reduce(json, state) {
    let data = _.get(json, 'contact-update', false);
    if (data) {
      this.create(data, state);
      this.delete(data, state);
      this.delete(data, state);
    }
  }

  create(json, state) {
    let data = _.get(json, 'create', false);
    if (data) {
      state.contacts[data.path] = {};
    }
  }

  delete(json, state) {
    let data = _.get(json, 'delete', false);
    if (data) {
      delete state.contacts[data.path];
    }
  }
  
}

