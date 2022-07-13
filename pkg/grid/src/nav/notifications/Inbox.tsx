import React, { useEffect } from 'react';
import { HarkLid, Notification } from '@urbit/api';
import { BasicNotification } from './BasicNotification';
import { useNotifications } from '../../state/notifications';
import { useHarkStore } from '../../state/hark';
import { OnboardingNotification } from './OnboardingNotification';

function renderNotification(notification: Notification, key: string, lid: HarkLid) {
  // Special casing
  if (notification.bin.place.desk === window.desk) {
    if (notification.bin.place.path === '/onboard') {
      return <OnboardingNotification key={key} lid={lid} />;
    }
  }
  return <BasicNotification key={key} notification={notification} lid={lid} />;
}

const Empty = () => (
  <section className="flex justify-center items-center min-h-[40vh] text-gray-400 space-y-2">
    <span className="h4">All clear!</span>
  </section>
);

export const Inbox = ({ archived = false }) => {
  const { unseen, seen } = useNotifications();
  const archive = useHarkStore((s) => s.archive);

  useEffect(() => {
    useHarkStore.getState().getMore();
  }, [archived]);

  if (archived ? archive.size === 0 : Object.keys({ ...seen, ...unseen }).length === 0) {
    return <Empty />;
  }

  return (
    <div className="text-gray-400 space-y-2 overflow-y-auto">
      {archived ? (
        Array.from(archive).map(([key, box]) => {
          return Object.entries(box)
            .sort(([, a], [, b]) => b.time - a.time)
            .map(([binId, n]) =>
              renderNotification(n, `${key.toString()}-${binId}`, { time: key.toString() })
            );
        })
      ) : (
        <>
          <header>Unseen</header>
          <section className="space-y-2">
            {Object.entries(unseen)
              .sort(([, a], [, b]) => b.time - a.time)
              .map(([binId, n]) => renderNotification(n, `unseen-${binId}`, { unseen: null }))}
          </section>
          <header>Seen</header>
          <section className="space-y-2">
            {Object.entries(seen)
              .sort(([, a], [, b]) => b.time - a.time)
              .map(([binId, n]) => renderNotification(n, `seen-${binId}`, { seen: null }))}
          </section>
        </>
      )}
    </div>
  );
};
