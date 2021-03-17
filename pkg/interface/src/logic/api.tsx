import { memoize } from "lodash";

import Urbit, { UrbitInterface } from '@urbit/http-api';
import { inviteSubscription } from "./reducers/invite-update";
import { metadataSubscription } from "./reducers/metadata-update";
import { contactSubscription } from "./reducers/contact-update";
import { groupSubscription } from "./reducers/group-update";
import { groupViewSubscription } from "./reducers/group-view";
import { graphSubscription, graphUpdateSubscription } from "./reducers/graph-update";
import React from "react";
import { harkGraphHookSubscription, harkGroupHookSubscription, harkSubscription } from "./reducers/hark-update";
import useLocalState from "./state/local";
import { launchSubscription, weatherSubscription } from "./reducers/launch-update";
import { settingsSubscription } from "./reducers/settings-update";
import { handleApiError } from "./lib/subscriptionHandlers";
import { s3Subscription } from "./reducers/s3-update";

const useApi = memoize((): UrbitInterface => {
  useLocalState.setState({ connection: 'reconnecting' });
  const channel: UrbitInterface = new Urbit(window.location.origin, '');
  // channel.verbose = true;
  channel.ship = window.ship;
  channel.subscribe(graphSubscription(channel));
  channel.subscribe(graphUpdateSubscription(channel));
  channel.subscribe(harkSubscription(channel));
  channel.subscribe(harkGraphHookSubscription(channel));
  channel.subscribe(harkGroupHookSubscription(channel));
  channel.subscribe(inviteSubscription(channel));
  channel.subscribe(metadataSubscription(channel));
  channel.subscribe(contactSubscription(channel));
  channel.subscribe({ app: 'contact-pull-hook', path: '/nacks' });
  channel.subscribe(groupSubscription(channel));
  channel.subscribe(groupViewSubscription(channel));
  channel.subscribe(launchSubscription(channel));
  channel.subscribe(weatherSubscription(channel));
  channel.subscribe(settingsSubscription(channel));
  channel.subscribe(s3Subscription(channel));
  // TODO make sure gcp is in here
  channel.onError = handleApiError(channel);
  useLocalState.setState({ connection: 'connected' });
  (window as any).channel = channel;
  return channel;
});

export const withApi = (Component: any) => {
  return React.forwardRef((props, ref) => {
    const api = useApi();
    return <Component ref={ref} {...props} api={api}  />
  });
}

export default useApi;