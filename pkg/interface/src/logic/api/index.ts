import Urbit from '@urbit/http-api';
import useHarkState from '~/logic/state/hark';

const api = new Urbit('', '');
api.ship = window.ship;
api.verbose = true;
console.log(api);

// @ts-ignore TODO window typings
window.api = api;

export const bootstrapApi = async () => {
  console.log('a');
  await api.poke({ app: 'hood', mark: 'helm-hi', json: 'opening airlock' });

  console.log('b');
  await api.eventSource();
  console.log('c');
  [useHarkState].forEach((state) => {
    state.getState().initialize(api);
    console.log('initialized');
  });
};

export default api;
