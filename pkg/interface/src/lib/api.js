import store from '../apps/chat/store.js';

class UrbitApi {
  setSelected(selected) {
    if (window.location.href.includes('~chat')) {
      store.handleEvent({
        data: {
          local: {
            selected: selected
          }
        }
      });
    }
  }
}

const api = new UrbitApi();
export default api;
