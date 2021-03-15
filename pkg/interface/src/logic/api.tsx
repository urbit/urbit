import { memoize } from "lodash";

import Urbit, { UrbitInterface } from '@urbit/http-api';
import { inviteSubscription } from "./reducers/invite-update";
import { metadataSubscription } from "./reducers/metadata-update";
import { contactSubscription } from "./reducers/contact-update";
import { groupSubscription } from "./reducers/group-update";
import { groupViewSubscription } from "./reducers/group-view";
import { graphSubscription } from "./reducers/graph-update";
import React from "react";
import { harkGraphHookSubscription, harkGroupHookSubscription, harkSubscription } from "./reducers/hark-update";
import useLocalState from "./state/local";
import { launchSubscription } from "./reducers/launch-update";
import { settingsSubscription } from "./reducers/settings-update";
import { handleApiError } from "./lib/subscriptionHandlers";

// import graphSubscription from '../subscription/graph';
// import groupSubscription, { groupViewSubscription } from '../subscription/groups';
// import harkSubscription, { harkGraphHookSubscription, harkGroupHookSubscription } from '../subscription/hark';

const useApi = memoize((): UrbitInterface => {
  const channel: UrbitInterface = new Urbit(window.location.origin, '');
  channel.verbose = true;
  channel.ship = window.ship;
  channel.subscribe(inviteSubscription(channel));
  channel.subscribe(metadataSubscription(channel));
  channel.subscribe(contactSubscription(channel));
  channel.subscribe({ app: 'contact-pull-hook', path: '/nacks' });
  channel.subscribe(groupSubscription(channel));
  channel.subscribe(graphSubscription(channel));
  channel.subscribe(groupViewSubscription(channel));
  channel.subscribe(harkSubscription(channel));
  channel.subscribe(harkGraphHookSubscription(channel));
  channel.subscribe(harkGroupHookSubscription(channel));
  channel.subscribe(launchSubscription(channel));
  channel.subscribe(settingsSubscription(channel));
  channel.eventSource();
  useLocalState.setState({ connection: 'disconnected' });
  channel.onError = handleApiError(channel);
  return channel;
});

export const withApi = (Component: any) => {
  return React.forwardRef((props, ref) => {
    const api = useApi();
    return <Component ref={ref} {...props} api={api}  />
  });
}

export default useApi;