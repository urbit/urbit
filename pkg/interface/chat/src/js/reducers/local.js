import _ from 'lodash';

export class LocalReducer {
    reduce(json, state) {
        let data = _.get(json, 'local', false);
        if (data) {
            this.sidebarToggle(data, state);
        }
    }

    sidebarToggle(obj, state) {
        let data = _.has(obj, 'sidebarToggle', false);
        if (data) {
            state.sidebarShown = obj.sidebarToggle;
        }
    }
}