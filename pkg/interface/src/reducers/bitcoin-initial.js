import _ from 'lodash';


export class WalletInitialReducer {
    reduce(json, state) {
        let data = _.get(json, 'initial', false);
        if (data) {
            state.hasXPub = (data !== "");
            state.xpubkey = data;
        }
    }
}
