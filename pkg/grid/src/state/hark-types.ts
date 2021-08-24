/**
 * I know this doesn't match our current hark type scheme, but since we're talking
 * about changing that I decided to just throw something together to at least test
 * this flow for updates.
 */

export interface SystemNotification {
  type: 'system-updates-blocked';
  charges: string[];
}

export interface BasicNotification {
  type: 'basic';
  time: string;
  message: string;
}

export type Notification = BasicNotification | SystemNotification;
