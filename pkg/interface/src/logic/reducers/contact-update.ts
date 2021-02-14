import _ from 'lodash';
import { StoreState } from '../../store/type';
import { Cage } from '~/types/cage';
import { ContactUpdate } from '@urbit/api/contacts';
import { resourceAsPath } from '../lib/util';

type ContactState  = Pick<StoreState, 'contacts'>;

export const ContactReducer = (json, state) => {
  const data = _.get(json, 'contact-update', false);
  if (data) {
    initial(data, state);
    add(data, state);
    remove(data, state);
    edit(data, state);
    setPublic(data, state);
  }

  // TODO: better isolation
  const res = _.get(json, 'resource', false);
  if(res) {
    state.nackedContacts = state.nackedContacts.add(`~${res.ship}`);
  }
};

const initial = (json: ContactUpdate, state: S) => {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.contacts = data.rolodex;
    state.isContactPublic = data['is-public'];
  }
};

const  add = (json: ContactUpdate, state: S) => {
  const data = _.get(json, 'add', false);
  if (data) {
    state.contacts[data.ship] = data.contact;
  }
};

const remove = (json: ContactUpdate, state: S) => {
  const data = _.get(json, 'remove', false);
  if (
    data &&
    (data.ship in state.contacts)
  ) {
    delete state.contacts[data.ship];
  }
};

const edit = (json: ContactUpdate, state: S) => {
  const data = _.get(json, 'edit', false);
  const ship = `~${data.ship}`;
  if (
    data &&
    (ship in state.contacts)
  ) {
    const [field] = Object.keys(data['edit-field']);
    if (!field) {
      return;
    }

    const value = data['edit-field'][field];
    
    if(field === 'add-group') {
      state.contacts[ship].groups.push(value);
    } else if (field === 'remove-group') {
      state.contacts[ship].groups =
        state.contacts[ship].groups.filter(g => g !== value);
    } else {
      state.contacts[ship][field] = value;
    }
  }
};

const setPublic = (json: ContactUpdate, state: S) => {
  const data = _.get(json, 'set-public', state.isContactPublic);
  state.isContactPublic = data;
};

