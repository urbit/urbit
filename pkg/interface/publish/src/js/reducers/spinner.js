import _ from 'lodash';

export class SpinnerReducer {
  reduce(json, state){
    if (json.spinner == undefined) {
      return;
    } else if (json.spinner == true) {
      state.spinner = true;
    } else if (json.spinner == false) {
      state.spinner = false;
    }
    return;
  }
}
