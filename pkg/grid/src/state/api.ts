import Urbit from '@urbit/http-api';

declare global {
  interface Window {
    ship: string;
  }
}

const api =
  import.meta.env.MODE === 'mock'
    ? {
        poke: () => {},
        subscribe: () => {},
        subscribeOnce: () => {},
        ship: ''
      }
    : new Urbit('', '');

api.ship = window.ship;
export default api;
