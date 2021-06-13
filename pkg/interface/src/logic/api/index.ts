import Urbit from '@urbit/http-api';
const api = new Urbit('', '');
api.ship = window.ship;
api.verbose = true;
// @ts-ignore TODO window typings
window.api = api;

export default api;
