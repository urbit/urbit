import _ from 'lodash';


export class UpdateReducer {
    reduce(json, state) {
    const alerts = _.get(json, 'alerts', false);
            if (alerts) {
                state.alerts = alerts;
                state.alertsLoaded = true

            }

            const stations = _.get(json, "stations", false);
            if (stations) {
                state.stations = stations;
                state.stationsLoaded = true

            }

            const routes = _.get(json, "routes", false);
            if (routes) {
               
                state.routes = routes;
                state.routesLoaded = true;
            }
            const facilities = _.get(json, "facilities", false);
            if (facilities) {
                console.log('Got em')
                state.facilities = facilities;
                state.facilitiesLoaded = true;
            }
            
            
        }
    
}
