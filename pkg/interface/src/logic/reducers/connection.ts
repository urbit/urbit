import _ from 'lodash';
import { StoreState } from '../../store/type';
import { Cage } from '~/types/cage';

type LocalState = Pick<StoreState, 'connection'>;

export default class ConnectionReducer<S extends LocalState> {
    reduce(json: Cage, state: S) {
      if('connection' in json && json.connection) {
        console.log(`Conn: ${json.connection}`);
        state.connection = json.connection;
      }
    }
}
