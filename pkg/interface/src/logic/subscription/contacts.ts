import _ from 'lodash';
import { compose } from 'lodash/fp';

import { ContactUpdate } from '@urbit/api';
import { SubscriptionRequestInterface, UrbitInterface } from '@urbit/http-api';

import useContactState, { ContactState } from '~/logic/state/contacts';

const initial = (json: ContactUpdate, state: ContactState): ContactState => {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.contacts = data.rolodex;
    state.isContactPublic = data['is-public'];
  }
  return state;
}

const add = (json: ContactUpdate, state: ContactState): ContactState => {
  const data = _.get(json, 'add', false);
  if (data) {
    state.contacts[data.ship] = data.contact;
  }
  return state;
}

const remove = (json: ContactUpdate, state: ContactState): ContactState => {
  const data = _.get(json, 'remove', false);
  if (
    data &&
    (data.ship in state.contacts)
  ) {
    delete state.contacts[data.ship];
  }
  return state;
}

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
}

export const contactReducer = (message) => {
  useContactState.setState(
    compose([
      initial,
      add,
      remove,
      edit,
    ].map(reducer => reducer.bind(reducer, message['contact-update']))
    )(useContactState.getState())
  );
};

const contactSubscription = (channel: UrbitInterface): SubscriptionRequestInterface => {
  const event = contactReducer;
  const err = (message) => {
    console.error(message);
    channel.subscribe(contactSubscription(channel));
  };
  const quit = (message) => {
    console.error(message);
    channel.subscribe(contactSubscription(channel));
  };
  return {
    app: 'contact-view',
    path: '/primary',
    event, err, quit
  };
};

export default contactSubscription;