import { UpdateReducer } from '/reducers/update';


class Store {
    constructor() {
        this.state = {}

        this.updateReducer = new UpdateReducer();
        this.setState = () => { };
    }

    setStateHandler(setState) {
        console.log("Calling setStateHandler");
        this.setState = setState;
    }

    handleEvent(data) {
        console.log("Handling event");
        console.log(data);
        let json = data.data;

        this.updateReducer.reduce(json, this.state);
        this.setState(this.state);
    }
}

export let store = new Store();
window.store = store;
