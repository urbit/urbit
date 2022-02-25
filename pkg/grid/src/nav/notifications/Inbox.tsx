import React, { useEffect } from 'react';
import { HarkLid, Notification } from '@urbit/api';
import { BasicNotification } from './BasicNotification';
import { BaseBlockedNotification, RuntimeLagNotification } from './SystemNotification';
import { useNotifications } from '../../state/notifications';
import { useHarkStore } from '../../state/hark';
import { OnboardingNotification } from './OnboardingNotification';
import moment from 'moment';

function renderNotification(notification: Notification, key: string, lid: HarkLid) {
  // Special casing
  if (notification.bin.place.desk === window.desk) {
    if (notification.bin.place.path === '/lag') {
      return <RuntimeLagNotification key={key} />;
    }
    if (notification.bin.path === '/blocked' && notification.bin.place.path === '/desk/base') {
      return <BaseBlockedNotification key={key} />;
    }
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
  }, []);

  useEffect(() => {
    return () => {
      useHarkStore.getState().opened();
    };

  }, []);

  if (false && archived ? archive.size === 0 : Object.keys({ ...seen, ...unseen }).length === 0) {
    //return <Empty />;
  }

  return (
    <div className="text-gray-400 space-y-2 overflow-y-auto">
      {Object.entries(unseen).length > 0 ? (
        <>
          <header>Unread</header>
          <section className="space-y-2">
            {Object.entries(unseen)
              .sort(([, a], [, b]) => b.time - a.time)
              .map(([binId, n]) => renderNotification(n, `seen-${binId}`, { seen: null }))}
          </section>
        </>
      ) : null}

      {Array.from(archive).map(([key, box], idx) => (
        <>
          <header>{moment().subtract(idx, 'days').startOf('day').calendar()}</header>
          <section className="space-y-2">
            {Object.entries(box)
              .sort(([, a], [, b]) => b.time - a.time)
              .map(([binId, n]) =>
                renderNotification(n, `${key.toString()}-${binId}`, { time: key.toString() })
              )}
          </section>
        </>
      ))}
    </div>
  );
};
