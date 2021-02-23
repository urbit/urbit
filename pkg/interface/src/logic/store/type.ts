import { Path } from '@urbit/api';
import { Invites } from '@urbit/api/invite';
import { Associations } from '@urbit/api/metadata';
import { Rolodex } from '@urbit/api/contacts';
import { Groups } from '@urbit/api/groups';
import { S3State } from '~/types/s3-update';
import { LaunchState, WeatherState } from '~/types/launch-update';
import { ConnectionStatus } from '~/types/connection';
import { Graphs } from '@urbit/api/graph';
import {
  JoinRequests,
  Patp
} from '@urbit/api';

export interface StoreState {
  // local state
  connection: ConnectionStatus;
  baseHash: string | null;

  s3: S3State;

  // App specific states
  //  launch state
  launch: LaunchState;
  weather: WeatherState | {} | null;
  userLocation: string | null;

}
