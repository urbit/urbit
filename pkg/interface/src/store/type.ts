import { Inbox, Envelope } from '../types/chat-update';
import { ChatHookUpdate } from '../types/chat-hook-update';
import { Path } from '../types/noun';
import { Invites } from '../types/invite-update';
import { SelectedGroup } from '../types/local-update';
import { Associations } from '../types/metadata-update';
import { Rolodex } from '../types/contact-update';
import { Notebooks } from '../types/publish-update';
import { Groups } from '../types/group-update';
import { S3State } from '../types/s3-update';
import { Permissions } from '../types/permission-update';
import { LaunchState, WeatherState } from '../types/launch-update';
import { LinkComments, LinkCollections, LinkSeen } from '../types/link-update';

export interface StoreState {
  // local state
  sidebarShown: boolean;
  selectedGroups: SelectedGroup[];
  // invite state
  invites: Invites;
  // metadata state
  associations: Associations;
  // contact state
  contacts: Rolodex;
  // groups state
  groups: Groups;
  groupKeys: Set<Path>;
  permissions: Permissions;
  s3: S3State;


  // App specific states
  //  launch state
  launch: LaunchState;
  weather: WeatherState | {} | null;
  userLocation: string | null;

  //  links state
  linksSeen: LinkSeen;
  linkListening: Set<Path>;
  links: LinkCollections;
  linkComments: LinkComments;

  // publish state
  notebooks: Notebooks;

  // Chat state
  chatInitialized: boolean;
  chatSynced: ChatHookUpdate | null;
  inbox: Inbox;
  pendingMessages: Map<Path, Envelope[]>;
}
