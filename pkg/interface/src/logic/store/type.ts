import { Path } from '@urbit/api';
import { Invites } from '@urbit/api/invite';
import { Associations } from '@urbit/api/metadata';
import { Rolodex } from '@urbit/api/contacts';
import { Groups } from '@urbit/api/groups';
import { StorageState } from '~/types/storage-state';
import { LaunchState, WeatherState } from '~/types/launch-update';
import { ConnectionStatus } from '~/types/connection';
import { Graphs } from '@urbit/api/graph';
import {
  Notifications,
  NotificationGraphConfig,
  GroupNotificationsConfig,
  Unreads,
  JoinRequests,
  Patp
} from '@urbit/api';

export interface StoreState {
  // local state
  connection: ConnectionStatus;
  baseHash: string | null;

  // invite state
  invites: Invites;
  // metadata state
  associations: Associations;
  // contact state
  contacts: Rolodex;
  // groups state
  groups: Groups;
  groupKeys: Set<Path>;
  nackedContacts: Set<Patp>
  storage: StorageState;
  graphs: Graphs;
  graphKeys: Set<string>;

  // App specific states
  //  launch state
  launch: LaunchState;
  weather: WeatherState | {} | null;
  userLocation: string | null;

  archivedNotifications: Notifications;
  notifications: Notifications;
  notificationsGraphConfig: NotificationGraphConfig;
  notificationsGroupConfig: GroupNotificationsConfig;
  notificationsCount: number,
  unreads: Unreads;
  doNotDisturb: boolean;
  pendingJoin: JoinRequests;
}
