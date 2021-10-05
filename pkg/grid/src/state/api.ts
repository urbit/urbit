import Urbit from '@urbit/http-api';
import { useMockData } from './util';

declare global {
  interface Window {
    ship: string;
  }
}

const api = useMockData
  ? ({
      poke: async () => {},
      subscribe: async () => {},
      subscribeOnce: async () => {},
      ship: '',
      scry: async () => {}
    } as unknown as Urbit)
  : new Urbit('', '');
if (useMockData) {
  api.verbose = true;
}
api.ship = useMockData ? 'dopzod' : window.ship;

export default api;
