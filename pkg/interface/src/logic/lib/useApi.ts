import Urbit from '@urbit/http-api';

import { memoize } from "lodash";
import inviteSubscription from '../subscription/invite';

const useApi = memoize(async () => {
  const channel = new Urbit(window.location.origin, '');
  channel.verbose = true;
  channel.ship = window.ship;
  channel.subscribe(inviteSubscription(channel));
  await channel.poke({ app: 'hood', mark: 'helm-hi', json: 'Loading Landscape' });
  await channel.eventSource(); 
  return channel;
});

export default useApi;