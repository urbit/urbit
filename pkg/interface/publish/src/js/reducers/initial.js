import _ from 'lodash';

export class InitialReducer {
  reduce(json, state) {
    state.notebooks = json.notebooks || null;
  }
}
