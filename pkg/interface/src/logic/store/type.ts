import { Inbox, Envelope } from '~/types/chat-update';
import { ChatHookUpdate } from '~/types/chat-hook-update';
import { Path } from '~/types/noun';
import { Invites } from '~/types/invite-update';
import { Associations } from '~/types/metadata-update';
import { Rolodex } from '~/types/contact-update';
import { Notebooks } from '~/types/publish-update';
import { Groups } from '~/types/group-update';
import { S3State } from '~/types/s3-update';
import { LaunchState, WeatherState } from '~/types/launch-update';
import { ConnectionStatus } from '~/types/connection';
import { BackgroundConfig, LocalUpdateRemoteContentPolicy } from '~/types/local-update';
import {Graphs} from '~/types/graph-update';

export interface StoreState {
  // local state
  sidebarShown: boolean;
  omniboxShown: boolean;
  suspendedFocus: HTMLInputElement | null;
  dark: boolean;
  connection: ConnectionStatus;
  baseHash: string | null;
  background: BackgroundConfig;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  hideAvatars: boolean;
  hideNicknames: boolean;

  // invite state
  invites: Invites;
  // metadata state
  associations: Associations;
  // contact state
  contacts: Rolodex;
  // groups state
  groups: Groups;
  groupKeys: Set<Path>;
  s3: S3State;
  graphs: Graphs;
  graphKeys: Set<string>;


  // App specific states
  //  launch state
  launch: LaunchState;
  weather: WeatherState | {} | null;
  userLocation: string | null;

  // publish state
  notebooks: Notebooks;

  // Chat state
  chatInitialized: boolean;
  chatSynced: ChatHookUpdate | null;
  inbox: Inbox;
  pendingMessages: Map<Path, Envelope[]>;
}
