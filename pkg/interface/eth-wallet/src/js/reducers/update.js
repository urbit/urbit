import _ from 'lodash';


export class UpdateReducer {
    reduce(json, state) {
        let data = _.get(json, 'update', false);
        if (data) {
            this.reduceInbox(_.get(data, 'inbox', false), state);
        }
    }

    reduceInbox(inbox, state) {
        if (inbox) {
            state.inbox = inbox;
        }
    }
}
