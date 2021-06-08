export type SidebarItemStatus =
  | 'unread'
  | 'notification'
  | 'unsubscribed'
  | 'disconnected'
  | 'loading';

export type SidebarSort = 'asc' | 'lastUpdated';

export interface SidebarListConfig {
  sortBy: SidebarSort;
  hideUnjoined: boolean;
}

export interface SidebarAppConfig {
  getStatus: (appPath: string) => SidebarItemStatus | undefined;
  lastUpdated: (appPath: string) => number;
}

export type SidebarAppConfigs = {
  graph: SidebarAppConfig;
};
