import Urbit from '@urbit/http-api';

// @ts-ignore TODO window typings
const api = new Urbit('', '', window.desk);
api.ship = window.ship;
// api.verbose = true;
// @ts-ignore TODO window typings
window.api = api;

export default api;
