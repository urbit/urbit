import _ from 'lodash';

export default class LocalReducer {
    reduce(json, state) {
        const data = _.get(json, 'local', false);
        if (data) {
            this.sidebarToggle(data, state);
            this.setSelected(data, state);
        }
    }

    sidebarToggle(obj, state) {
        const data = _.has(obj, 'sidebarToggle', false);
        if (data) {
            state.sidebarShown = obj.sidebarToggle;
        }
    }

    setSelected(obj, state) {
      const data = _.has(obj, 'selected', false);
      if (data) {
        state.selectedGroups = obj.selected;
      }
    }
}
