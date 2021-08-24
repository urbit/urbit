import React, { useEffect } from 'react';
import { Link } from 'react-router-dom';
import { Button } from '../components/Button';
import { useHarkStore } from '../state/hark';
import { Notification } from '../state/hark-types';
import { useLeapStore } from './Nav';
import { BasicNotification } from './notifications/BasicNotification';
import { SystemNotification } from './notifications/SystemNotification';

function renderNotification(notification: Notification, key: string) {
  if (notification.type === 'system-updates-blocked') {
    return <SystemNotification key={key} notification={notification} />;
  }

  return <BasicNotification key={key} notification={notification} />;
}

const Empty = () => (
  <section className="flex justify-center items-center min-h-[480px] text-gray-400 space-y-2">
    <span className="h4">All clear!</span>
  </section>
);

export const Notifications = () => {
  const select = useLeapStore((s) => s.select);
  const notifications = useHarkStore((s) => s.notifications);
  const hasNotifications = notifications.length > 0;

  useEffect(() => {
    select('Notifications');
  }, []);

  return (
    <div className="p-4 md:p-8">
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

      {!hasNotifications && <Empty />}
      {hasNotifications && (
        <section className="min-h-[480px] text-gray-400 space-y-2">
          {notifications.map((n, index) => renderNotification(n, index.toString()))}
        </section>
      )}
    </div>
  );
};
