import React, { useEffect } from 'react';
import { Link } from 'react-router-dom';
import { HarkLid, Notification } from '@urbit/api';
import { useLeapStore } from '../Nav';
import { Button } from '../../components/Button';
import { BasicNotification } from './BasicNotification';
import { BaseBlockedNotification, RuntimeLagNotification } from './SystemNotification';
import { useNotifications } from '../../state/notifications';
import { useHarkStore } from '../../state/hark';
import { OnboardingNotification } from './OnboardingNotification';

function renderNotification(notification: Notification, key: string, lid: HarkLid) {
  // Special casing
  if (notification.bin.place.desk === window.desk) {
    if (notification.bin.place.path === '/lag') {
      return <RuntimeLagNotification key={key} />;
    }
    if (notification.bin.place.path === '/blocked') {
      return <BaseBlockedNotification key={key} />;
    }
    if (notification.bin.place.path === '/onboard') {
      return <OnboardingNotification key={key} unread={false} />;
    }
  }
  return <BasicNotification key={key} notification={notification} lid={lid} />;
}

const Empty = () => (
  <section className="flex justify-center items-center min-h-[480px] text-gray-400 space-y-2">
    <span className="h4">All clear!</span>
  </section>
);

export const Inbox = ({ archived = false }) => {
  const select = useLeapStore((s) => s.select);
  const { unseen, seen, hasAnyNotifications } = useNotifications();
  const archive = useHarkStore((s) => s.archive);
  const markAllAsRead = () => {};

  useEffect(() => {
    useHarkStore.getState().getMore();

  }, [archived]);

  useEffect(() => {
    select('Notifications');
    const { getMore } = useHarkStore.getState();
    getMore();

    function visibilitychange() {
      setTimeout(() => useHarkStore.getState().opened(), 100);
    }
    document.addEventListener('visibilitychange', visibilitychange);

    return () => {
      document.removeEventListener('visibilitychange', visibilitychange);
      visibilitychange();
    };
  }, []);
  // const select = useLeapStore((s) => s.select);

  if (false) {
    return <Empty />;
  }

  return (
    <div className="text-gray-400 space-y-2 overflow-y-auto">
      {archived ? (
        Array.from(archive).map(([key, box]) => {
          return Object.entries(box)
            .sort(([, a], [, b]) => b.time - a.time)
            .map(([binId, n], index) => renderNotification(n, `${key.toString}-${binId}`, { time: key.toString() }));
        })
      ) : (
        <>
          <header>Unseen</header>
          <section className="space-y-2">
            {Object.entries(unseen)
              .sort(([, a], [, b]) => b.time - a.time)
              .map(([binId, n], index) => renderNotification(n, `unseen-${binId}`, { unseen: null }))}
          </section>
          <header>Seen</header>
          <section className="space-y-2">
            {Object.entries(seen)
              .sort(([, a], [, b]) => b.time - a.time)
              .map(([binId, n], index) => renderNotification(n, `seen-${binId}`, { seen: null }))}
          </section>
        </>
      )}
    </div>
  );
};
