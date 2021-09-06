
export interface HarkStats {
  count: number;
  each: string[];
  last: number;
}

export interface Timebox {
  [binId: string]: Notification[];
}

export type HarkContent = { ship: string; } | { text: string; };

export interface HarkBody {
  title: HarkContent[];
  time: string;
  content: HarkContent[];
  author?: string;
  link: string;
}

export interface HarkPlace {
  desk: string;
  path: string;
}

export interface HarkBin {
  path: string;
  place: HarkPlace;
}

export type HarkBinId = string;
export interface Notification {
  bin: HarkBin;
  time: number;
  body: HarkBody[];
}

export interface NotificationGraphConfig {
  watchOnSelf: boolean;
  mentions: boolean;
  watching: WatchedIndex[]
}

export interface Unreads {
  [path: string]: HarkStats;
}

interface WatchedIndex {
  graph: string;
  index: string;
}
export type GroupNotificationsConfig = string[];
