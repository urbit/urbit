import _ from 'lodash';


export default class WalletInitialReducer {
    reduce(json, state) {
        let data = _.get(json, 'xpubkey', false);
        if (data) {
            state.xpubkey = data;
        }
    }
}
