import { ContactUpdate, GroupUpdate, InviteUpdate, MetadataUpdate } from '@urbit/api';
import { SettingsUpdate } from '@urbit/api/settings';
import { ConnectionStatus } from './connection';
import { LaunchUpdate, WeatherState } from './launch-update';
import { LocalUpdate } from './local-update';

interface MarksToTypes {
  readonly json: any;
  readonly 'contact-update': ContactUpdate;
  readonly 'invite-update': InviteUpdate;
  readonly 'metadata-update': MetadataUpdate;
  readonly groupUpdate: GroupUpdate;
  readonly 'launch-update': LaunchUpdate;
  readonly 'settings-event': SettingsUpdate;
  // not really marks but w/e
  readonly 'local': LocalUpdate;
  readonly 'weather': WeatherState | {};
  readonly 'location': string;
  readonly 'connection': ConnectionStatus;
}

export type Cage = Partial<MarksToTypes>;

export type Mark = keyof MarksToTypes;
