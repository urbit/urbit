import _ from 'lodash';


export class InitialReducer {
    reduce(json, state) {
        let data = _.get(json, 'update', false);
        if (data) {
            const stations = _.get(data, 'alerts', false);
            if (stations) {
                state.alerts = stations;
            }
         }
    }
}
