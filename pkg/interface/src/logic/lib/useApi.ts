import { memoize } from "lodash";

import Urbit, { UrbitInterface } from '@urbit/http-api';

import contactSubscription from '../subscription/contacts';
import graphSubscription from '../subscription/graph';
import groupSubscription, { groupViewSubscription } from '../subscription/groups';
import harkSubscription, { harkGraphHookSubscription, harkGroupHookSubscription } from '../subscription/hark';
import inviteSubscription from '../subscription/invite';
import metadataSubscription from '../subscription/metadata';

const useApi = memoize((): UrbitInterface => {
  const channel: UrbitInterface = new Urbit(window.location.origin, '');
  // channel.verbose = true;
  channel.ship = window.ship;
  channel.subscribe(inviteSubscription(channel));
  channel.subscribe(contactSubscription(channel));
  channel.subscribe(groupSubscription(channel));
  channel.subscribe(graphSubscription(channel));
  channel.subscribe(groupViewSubscription(channel));
  channel.subscribe(metadataSubscription(channel));
  channel.subscribe(harkSubscription(channel));
  channel.subscribe(harkGraphHookSubscription(channel));
  channel.subscribe(harkGroupHookSubscription(channel));
  channel.eventSource();
  return channel;
});

export default useApi;