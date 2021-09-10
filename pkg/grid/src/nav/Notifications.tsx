import React from 'react';
import { Link } from 'react-router-dom';
import { Button } from '../components/Button';
import { Notification } from '../state/hark-types';
import { BasicNotification } from './notifications/BasicNotification';
import {
  BaseBlockedNotification,
  RuntimeLagNotification
} from './notifications/SystemNotification';
import { useNotifications } from '../state/notifications';

function renderNotification(notification: Notification, key: string) {
  if (notification.type === 'system-updates-blocked') {
    return <BaseBlockedNotification key={key} notification={notification} />;
  }
  if (notification.type === 'runtime-lag') {
    return <RuntimeLagNotification key={key} />;
  }
  return <BasicNotification key={key} notification={notification} />;
}

const Empty = () => (
  <section className="flex justify-center items-center min-h-[480px] text-gray-400 space-y-2">
    <span className="h4">All clear!</span>
  </section>
);

export const Notifications = () => {
  // const select = useLeapStore((s) => s.select);
  const { notifications, systemNotifications, hasAnyNotifications } = useNotifications();

  // useEffect(() => {
  //   select('Notifications');
  // }, []);

  return (
    <div className="grid grid-rows-[auto,1fr] h-full p-4 md:p-8 overflow-hidden">
      <header className="space-x-2 mb-8">
        <Button variant="secondary" className="py-1.5 px-6 rounded-full">
          Mark All as Read
        </Button>
        <Button
          as={Link}
          variant="secondary"
          to="/leap/system-preferences/notifications"
          className="py-1.5 px-6 rounded-full"
        >
          Notification Settings
        </Button>
      </header>

      {!hasAnyNotifications && <Empty />}
      {hasAnyNotifications && (
        <section className="text-gray-400 space-y-2 overflow-y-auto">
          {notifications.map((n, index) => renderNotification(n, index.toString()))}
          {systemNotifications.map((n, index) =>
            renderNotification(n, (notifications.length + index).toString())
          )}
        </section>
      )}
    </div>
  );
};
