import _ from 'lodash';
import { compose } from 'lodash/fp';

import { ContactUpdate } from '@urbit/api';

import useContactState, { ContactState } from '../state/contact';
import { reduceState } from '../state/base';


export const ContactReducer = (json) => {
  const data: ContactUpdate = _.get(json, 'contact-update', false);
  if (data) {
    reduceState<ContactState, ContactUpdate>(useContactState, data, [
      initial,
      add,
      remove,
      edit,
      setPublic
    ]);
  }

  // TODO: better isolation
  const res = _.get(json, 'resource', false);
  if (res) {
    useContactState.setState({
      nackedContacts: useContactState.getState().nackedContacts.add(`~${res.ship}`)
    });
  }
};

const initial = (json: ContactUpdate, state: ContactState): ContactState => {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.contacts = data.rolodex;
    state.isContactPublic = data['is-public'];
  }
  return state;
};

const  add = (json: ContactUpdate, state: ContactState): ContactState => {
  const data = _.get(json, 'add', false);
  if (data) {
    state.contacts[data.ship] = data.contact;
  }
  return state;
};

const remove = (json: ContactUpdate, state: ContactState): ContactState => {
  const data = _.get(json, 'remove', false);
  if (
    data &&
    (data.ship in state.contacts)
  ) {
    delete state.contacts[data.ship];
  }
  return state;
};

const edit = (json: ContactUpdate, state: ContactState): ContactState => {
  const data = _.get(json, 'edit', false);
  const ship = `~${data.ship}`;
  if (
    data &&
    (ship in state.contacts)
  ) {
    const [field] = Object.keys(data['edit-field']);
    if (!field) {
      return state;
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
  return state;
};

const setPublic = (json: ContactUpdate, state: ContactState): ContactState => {
  const data = _.get(json, 'set-public', state.isContactPublic);
  state.isContactPublic = data;
  return state;
};

