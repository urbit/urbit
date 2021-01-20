import { InviteUpdate, InviteUpdateCreate, InviteUpdateDecline, InviteUpdateInitial } from '@urbit/api/invite/types';
import { SubscriptionRequestInterface } from '@urbit/http-api/src/types';
import { Partial } from 'aws-sdk/clients/cloudsearchdomain';
import _ from 'lodash';
import { create } from 'lodash';
import { compose } from 'lodash/fp';
import useInviteState, { InviteState } from '../state/invite';

const initial = (json: InviteUpdate, state: InviteState) => {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.invites = data;
  }
  return state;
}

const createInvite = (json: InviteUpdate, state: InviteState) => {
  const data = _.get(json, 'create', false);
  if (data) {
    state.invites[data] = {};
  }
  return state;
}

const deleteInvite = (json: InviteUpdate, state: InviteState) => {
  const data = _.get(json, 'delete', false);
  if (data) {
    delete state.invites[data];
  }
  return state;
}

const invite = (json: InviteUpdate, state: InviteState) => {
  const data = _.get(json, 'invite', false);
  if (data) {
    state.invites[data.term][data.uid] = data.invite;
  }
  return state;
}

const accepted = (json: InviteUpdate, state: InviteState) => {
  const data = _.get(json, 'accepted', false);
  if (data) {
    delete state.invites[data.term][data.uid];
  }
  return state;
}

const decline = (json: InviteUpdate, state: InviteState) => {
  const data = _.get(json, 'decline', false);
  if (data) {
    delete state.invites[data.term][data.uid];
  }
  return state;
}

const inviteSubscription = (channel): SubscriptionRequestInterface => {
  const event = (message) => {
    useInviteState.setState(
      compose([
        initial,
        createInvite,
        deleteInvite,
        invite,
        accepted,
        decline,
      ].map(reducer => reducer.bind(reducer, message['invite-update']))
      )(useInviteState.getState())
    );
  };
  const err = (message) => {
    console.error(message);
    channel.subscribe(inviteSubscription(channel));
  };
  const quit = (message) => {
    console.error(message);
    channel.subscribe(inviteSubscription(channel));
  };
  return {
    app: 'invite-store',
    path: '/all',
    event, err, quit
  };
};

export default inviteSubscription;