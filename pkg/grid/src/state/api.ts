import Urbit from '@urbit/http-api';

declare global {
  interface Window {
    ship: string;
  }
}

const api =
  import.meta.env.MODE === 'mock'
    ? {
        poke: async () => {},
        subscribe: async () => {},
        subscribeOnce: async () => {},
        ship: '',
        scry: async () => {}
      } as unknown as Urbit
    : new Urbit('', '');

api.ship = 'dopmet';
export default api;
