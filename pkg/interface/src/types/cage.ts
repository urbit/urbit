import { ChatUpdate } from "./chat-update";
import { ChatHookUpdate } from "./chat-hook-update";
import { ContactUpdate } from "./contact-update";
import { InviteUpdate } from "./invite-update";
import { LocalUpdate } from "./local-update";
import { MetadataUpdate } from "./metadata-update";
import { PublishUpdate } from './publish-update';
import { PublishResponse } from "./publish-response";
import { GroupUpdate } from "./group-update";
import { PermissionUpdate } from "./permission-update";
import { LaunchUpdate, WeatherState } from "./launch-update";
import { LinkListenUpdate } from './link-listen-update';
import { ConnectionStatus } from "./connection";

interface MarksToTypes {
  readonly json: any;
  readonly "chat-update": ChatUpdate;
  readonly "chat-hook-update": ChatHookUpdate;
  readonly "contact-update": ContactUpdate;
  readonly "invite-update": InviteUpdate;
  readonly "metadata-update": MetadataUpdate;
  readonly 'publish-update': PublishUpdate;
  readonly "publish-response": PublishResponse;
  readonly groupUpdate: GroupUpdate;
  readonly "permission-update": PermissionUpdate;
  readonly "launch-update": LaunchUpdate;
  readonly "link-listen-update": LinkListenUpdate;
  // not really marks but w/e
  readonly 'local': LocalUpdate;
  readonly 'weather': WeatherState | {};
  readonly 'location': string;
  readonly 'connection': ConnectionStatus;
}

export type Cage = Partial<MarksToTypes>;

export type Mark = keyof MarksToTypes;
