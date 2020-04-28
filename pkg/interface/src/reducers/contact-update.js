import _ from 'lodash';

export default class ContactUpdateReducer {
  reduce(json, state) {
    const data = _.get(json, 'contact-update', false);
    if (data) {
      this.create(data, state);
      this.delete(data, state);
      this.add(data, state);
      this.remove(data, state);
      this.edit(data, state);
    }
  }

  create(json, state) {
    const data = _.get(json, 'create', false);
    if (data) {
      state.contacts[data.path] = {};
    }
  }

  delete(json, state) {
    const data = _.get(json, 'delete', false);
    if (data) {
      delete state.contacts[data.path];
    }
  }

  add(json, state) {
    const data = _.get(json, 'add', false);
    if (
      data &&
      (data.path in state.contacts)
    ) {
      state.contacts[data.path][data.ship] = data.contact;
    }
  }

  remove(json, state) {
    const data = _.get(json, 'remove', false);
    if (
      data &&
      (data.path in state.contacts) &&
      (data.ship in state.contacts[data.path])
    ) {
      delete state.contacts[data.path][data.ship];
    }
  }

  edit(json, state) {
    const data = _.get(json, 'edit', false);
    if (
      data &&
      (data.path in state.contacts) &&
      (data.ship in state.contacts[data.path])
    ) {
      const edit = Object.keys(data['edit-field']);
      if (edit.length !== 1) {
        return;
      }
      state.contacts[data.path][data.ship][edit[0]] =
        data['edit-field'][edit[0]];
    }
  }
}
