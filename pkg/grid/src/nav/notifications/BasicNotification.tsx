import React from 'react';
import { BasicNotification as BasicNotificationType } from '../../state/hark-types';

interface BasicNotificationProps {
  notification: BasicNotificationType;
}

export const BasicNotification = ({ notification }: BasicNotificationProps) => (
  <div>{notification.message}</div>
);
