import _ from "lodash";
import { Post } from "./graph-update";
import { GroupUpdate } from "./group-update";
import { BigIntOrderedMap } from "~/logic/lib/BigIntOrderedMap";
import { Envelope } from './chat-update';

export type GraphNotifDescription = "link" | "comment" | "note" | "mention";

export interface UnreadStats {
  unreads: Set<string> | number;
  notifications: number;
  last: number;
}

export interface GraphNotifIndex {
  graph: string;
  group: string;
  description: GraphNotifDescription;
  module: string;
  index: string;
}

export interface GroupNotifIndex {
  group: string;
  description: string;
}

export interface ChatNotifIndex {
  chat: string;
  mention: boolean;
}

export type NotifIndex =
  | { graph: GraphNotifIndex }
  | { group: GroupNotifIndex }
  | { chat: ChatNotifIndex };

export type GraphNotificationContents = Post[];

export type GroupNotificationContents = GroupUpdate[];

export type ChatNotificationContents = Envelope[];

export type NotificationContents =
  | { graph: GraphNotificationContents }
  | { group: GroupNotificationContents }
  | { chat: ChatNotificationContents };

export interface Notification {
  read: boolean;
  time: number;
  contents: NotificationContents;
}

export interface IndexedNotification {
  index: NotifIndex;
  notification: Notification;
}

export type Timebox = IndexedNotification[];

export type Notifications = BigIntOrderedMap<Timebox>;

export interface NotificationGraphConfig {
  watchOnSelf: boolean;
  mentions: boolean;
  watching: WatchedIndex[]
}

export interface Unreads {
  chat: Record<string, UnreadStats>;
  graph: Record<string, Record<string, UnreadStats>>;
  group: Record<string, UnreadStats>;
}

interface WatchedIndex {
  graph: string;
  index: string;
}
export type GroupNotificationsConfig = string[];
