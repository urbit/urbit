import React, { useEffect } from 'react';
import { Link } from 'react-router-dom';
import { Notification } from '@urbit/api';
import { useLeapStore } from './Nav';
import { Button } from '../components/Button';
import { BasicNotification } from './notifications/BasicNotification';
import {
  BaseBlockedNotification,
  RuntimeLagNotification
} from './notifications/SystemNotification';
import { useNotifications } from '../state/notifications';
import { useHarkStore } from '../state/hark';
import { OnboardingNotification } from './notifications/OnboardingNotification';

function renderNotification(notification: Notification, key: string, unread = false) {
  // Special casing
  if (notification.bin.place.desk === window.desk) {
    if (notification.bin.place.path === '/lag') {
      return <RuntimeLagNotification key={key} />;
    }
    if (notification.bin.place.path === '/blocked') {
      return <BaseBlockedNotification key={key} />;
    }
    if (notification.bin.place.path === '/onboard') {
      return <OnboardingNotification key={key} unread={unread} />;
    }
  }
  return <BasicNotification key={key} notification={notification} unread={unread} />;
}

const Empty = () => (
  <section className="flex justify-center items-center min-h-[480px] text-gray-400 space-y-2">
    <span className="h4">All clear!</span>
  </section>
);

export const Notifications = () => {
  const select = useLeapStore((s) => s.select);
  const { unreads, reads, hasAnyNotifications } = useNotifications();
  const markAllAsRead = () => {
    const { readAll } = useHarkStore.getState();
    readAll();
  };

  useEffect(() => {
    select('Notifications');
    const { getMore } = useHarkStore.getState();
    getMore();
  }, []);
  // const select = useLeapStore((s) => s.select);

  return (
    <div className="grid grid-rows-[auto,1fr] h-full p-4 md:p-8 overflow-hidden">
      <header className="space-x-2 mb-8">
        <Button onClick={markAllAsRead} variant="secondary" className="py-1.5 px-6 rounded-full">
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
          {unreads.map((n, index) => renderNotification(n, index.toString(), true))}
          {Array.from(reads)
            .map(([, nots]) => nots)
            .flat()
            .map((n, index) => renderNotification(n, `reads-${index}`))}
        </section>
      )}
    </div>
  );
};
