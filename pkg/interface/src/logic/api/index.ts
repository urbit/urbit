import Urbit from '@urbit/http-api';
import useHarkState from '~/logic/state/hark';
import useMetadataState from '~/logic/state/metadata';

const api = new Urbit('', '');
api.ship = window.ship;
api.verbose = true;
console.log(api);

// @ts-ignore TODO window typings
window.api = api;

export const bootstrapApi = async () => {
  await api.poke({ app: 'hood', mark: 'helm-hi', json: 'opening airlock' });

  await api.eventSource();
  [useHarkState, useMetadataState].forEach((state) => {
    state.getState().initialize(api);
  });
};

export default api;
