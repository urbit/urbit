import _ from 'lodash';


export class InitialReducer {
  reduce(json, state) {
  let data = _.get(json, 'contact-initial', false);
    if (data) {
      state.contacts = data;
    }

    data = _.get(json, 'group-initial', false);
    if (data) {
      for (let group in data) {
        state.groups[group] = new Set(data[group]);
      }
    }
  
  data = _.get(json, 'link', false);
  if (data) {
    let name = Object.keys(data)[0];
    let initial = {};
    initial[name] = {};
    initial[name]["total-pages"] = data[name]["total-pages"];
    initial[name]["total-items"] = data[name]["total-items"];
    initial[name]["page0"] = data[name]["page"];

    if (!!state.links[name]) {
      let origin = state.links[name];
      _.extend(initial[name], origin);
    } else {
      state.links[name] = {};
    }
    state.links[name] = initial[name];
  }
  }
}

