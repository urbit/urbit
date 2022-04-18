import Urbit from '@urbit/http-api';
import UrbitMock from '@urbit/mock-http-api';
import { useMockData } from './util';
import { handlers } from './mock-handlers';

declare global {
  interface Window {
    ship: string;
  }
}

const api = useMockData ? new UrbitMock('', '', handlers) : new Urbit('', '');
if (import.meta.env.DEV || useMockData) {
  api.verbose = true;
}
api.ship = useMockData ? 'dopzod' : window.ship;

export default api;
