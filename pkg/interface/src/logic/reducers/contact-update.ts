import _ from 'lodash';
import { StoreState } from '../../store/type';
import { Cage } from '~/types/cage';
import { ContactUpdate } from '~/types/contact-update';

type ContactState  = Pick<StoreState, 'contacts'>;

export const ContactReducer = (json, state) => {
  const data = _.get(json, 'contact-update', false);
  if (data) {
    console.log(data);
    initial(data, state);
    add(data, state);
    remove(data, state);
    edit(data, state);
    console.log(state);
  }
};

const initial = (json: ContactUpdate, state: S) => {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.contacts = data;
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
  if (
    data &&
    (data.ship in state.contacts)
  ) {
    const edit = Object.keys(data['edit-field']);
    if (edit.length !== 1) {
      return;
    }
    state.contacts[data.ship][edit[0]] = data['edit-field'][edit[0]];
  }
};
