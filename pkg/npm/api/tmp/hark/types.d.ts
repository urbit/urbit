export interface HarkStats {
    count: number;
    each: string[];
    last: number;
}
export interface Timebox {
    [binId: string]: Notification;
}
export declare type HarkContent = {
    ship: string;
} | {
    text: string;
};
export interface HarkBody {
    title: HarkContent[];
    time: string;
    content: HarkContent[];
    link: string;
    binned: string;
}
export interface HarkPlace {
    desk: string;
    path: string;
}
export interface HarkBin {
    path: string;
    place: HarkPlace;
}
export declare type HarkLid = {
    unseen: null;
} | {
    seen: null;
} | {
    time: string;
};
export declare type HarkBinId = string;
export interface Notification {
    bin: HarkBin;
    time: number;
    body: HarkBody[];
}
export interface NotificationGraphConfig {
    watchOnSelf: boolean;
    mentions: boolean;
    watching: WatchedIndex[];
}
export interface Unreads {
    [path: string]: HarkStats;
}
interface WatchedIndex {
    graph: string;
    index: string;
}
export declare type GroupNotificationsConfig = string[];
export {};
