import _ from 'lodash';


export default class WalletAddressReducer {
    reduce(json, state) {
        let data = _.get(json, 'address', false);
        if (data) {
            state.address = data;
        }
    }
}
