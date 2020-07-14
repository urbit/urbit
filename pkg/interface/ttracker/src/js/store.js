import { InitialReducer } from '/reducers/initial';
import { ConfigReducer } from '/reducers/config';
import { UpdateReducer } from '/reducers/update';


class Store {
    constructor() {
        this.state = {
            routes: {},
            stations: {},
            alerts: {},
            stops: {},
            alertsLoaded: false,
            stationsLoaded: false,
            routesLoaded: false
            
        }
       
        this.updateReducer = new UpdateReducer();
        this.setState = () => { };
    }

    setStateHandler(setState) {
        this.setState = setState;
    }

    handleEvent(data) {
        let json = data.data;
      
    
        this.updateReducer.reduce(json, this.state);

        this.setState(this.state);
    }
}

export let store = new Store();
window.store = store;