import { ConnectionStatus } from '~/types/connection';

export interface StoreState {
  // local state
  connection: ConnectionStatus;
}
