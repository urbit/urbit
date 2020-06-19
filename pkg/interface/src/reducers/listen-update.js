import _ from 'lodash';

export default class ListenUpdateReducer {
  reduce(json, state) {
    const data = _.get(json, 'link-listen-update', false);
    if (data) {
      this.listening(data, state);
      this.watch(data, state);
      this.leave(data, state);
    }
  }

  listening(json, state) {
    const data = _.get(json, 'listening', false);
    if (data) {
      state.listening = new Set(data);
    }
  }

  watch(json, state) {
    const data = _.get(json, 'watch', false);
    if (data) {
      state.listening.add(data);
    }
  }

  leave(json, state) {
    const data = _.get(json, 'leave', false);
    if (data) {
      state.listening.delete(data);
    }
  }
}

