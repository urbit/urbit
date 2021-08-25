import React, { useEffect } from 'react';
import { Link } from 'react-router-dom';
import { useLeapStore } from './Nav';
import { useBlockers, useLag } from '../state/kiln';
import { Button } from '../components/Button';
import { useHarkStore } from '../state/hark';
import { Notification } from '../state/hark-types';
import { BasicNotification } from './notifications/BasicNotification';
import {
  BaseBlockedNotification,
  RuntimeLagNotification
} from './notifications/SystemNotification';

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

function getSystemNotifications(lag: boolean, blockers: string[]) {
  const nots = [] as Notification[];
  if (lag) {
    nots.push({ type: 'runtime-lag' });
  }
  if (blockers.length > 0) {
    nots.push({ type: 'system-updates-blocked', desks: blockers });
  }
  return nots;
}

export const Notifications = () => {
  const select = useLeapStore((s) => s.select);
  const notifications = useHarkStore((s) => s.notifications);
  const blockers = useBlockers();
  const lag = useLag();
  const systemNotifications = getSystemNotifications(lag, blockers);
  const hasNotifications = notifications.length > 0 || systemNotifications.length > 0;

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
        <section className="min-h-[480px] text-gray-400 space-y-2 overflow-y-auto">
          {notifications.map((n, index) => renderNotification(n, index.toString()))}
          {systemNotifications.map((n, index) =>
            renderNotification(n, (notifications.length + index).toString())
          )}
        </section>
      )}
    </div>
  );
};
