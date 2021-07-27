import Urbit from '@urbit/http-api';
const api = new Urbit('', '', {
  ship: window.ship,
  managedChannel: true
});

// @ts-ignore TODO window typings
window.api = api;

export default api;
