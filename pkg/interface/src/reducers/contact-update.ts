import _ from 'lodash';
import { StoreState } from '../store/type';
import { Cage } from '../types/cage';
import { ContactUpdate } from '../types/contact-update';

type ContactState  = Pick<StoreState, 'contacts'>;

export default class ContactReducer<S extends ContactState>  {
  reduce(json: Cage, state: S) {
    const data = _.get(json, 'contact-update', false);
    if (data) {
      this.initial(data, state);
      this.create(data, state);
      this.delete(data, state);
      this.add(data, state);
      this.remove(data, state);
      this.edit(data, state);
    }
  }

  initial(json: ContactUpdate, state: S) {
    const data = _.get(json, 'initial', false);
    if (data) {
      state.contacts = data;
    }
  }

  create(json: ContactUpdate, state: S) {
    const data = _.get(json, 'create', false);
    if (data) {
      state.contacts[data.path] = {};
    }
  }

  delete(json: ContactUpdate, state: S) {
    const data = _.get(json, 'delete', false);
    if (data) {
      delete state.contacts[data.path];
    }
  }

  add(json: ContactUpdate, state: S) {
    const data = _.get(json, 'add', false);
    if (
      data &&
      (data.path in state.contacts)
    ) {
      state.contacts[data.path][data.ship] = data.contact;
    }
  }

  remove(json: ContactUpdate, state: S) {
    const data = _.get(json, 'remove', false);
    if (
      data &&
      (data.path in state.contacts) &&
      (data.ship in state.contacts[data.path])
    ) {
      delete state.contacts[data.path][data.ship];
    }
  }

  edit(json: ContactUpdate, state: S) {
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
