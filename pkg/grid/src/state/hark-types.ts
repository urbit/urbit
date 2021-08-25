/**
 * I know this doesn't match our current hark type scheme, but since we're talking
 * about changing that I decided to just throw something together to at least test
 * this flow for updates.
 */

export interface RuntimeLagNotification {
  type: 'runtime-lag';
}

export interface BaseBlockedNotification {
  type: 'system-updates-blocked';
  desks: string[];
}

export interface BasicNotification {
  type: 'basic';
  time: string;
  message: string;
}

export type Notification = BasicNotification | BaseBlockedNotification | RuntimeLagNotification;
