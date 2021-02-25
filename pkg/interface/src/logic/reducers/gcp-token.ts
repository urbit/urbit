import _ from 'lodash';
import {StoreState} from '../store/type';
import {GcpToken} from '../../types/gcp-state';

type GcpState = Pick<StoreState, 'gcp'>;

export default class GcpReducer<S extends GcpState>{
  reduce(json: Cage, state: S) {
    let data = json['gcp-token'];
    if (data) {
      this.setAccessKey(data, state);
    }
  }

  setAccessKey(json: GcpToken, state: S) {
    const data = _.get(json, 'accessKey');
    if (data) {
      state.gcp.accessKey = data;
    }
  }
}
