import React, { useEffect } from 'react';
import { Link, NavLink, Route, Switch } from 'react-router-dom';
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
import { Inbox } from './notifications/Inbox';

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
  const { unseen, seen, hasAnyNotifications } = useNotifications();
  const markAllAsRead = () => {
    const { readAll } = useHarkStore.getState();
    readAll();
  };

  useEffect(() => {
    select('Notifications');
    const { getMore } = useHarkStore.getState();
    getMore();

    function visibilitychange() {
      useHarkStore.getState().opened();
    }
    document.addEventListener('visibilitychange', visibilitychange);

    return () => {
      document.removeEventListener('visibilitychange', visibilitychange);
      useHarkStore.getState().opened();
    };
  }, []);
  // const select = useLeapStore((s) => s.select);

  return (
    <div className="grid grid-rows-[auto,1fr] h-full p-4 md:p-8 overflow-hidden">
      <header className="space-x-2 mb-8">
        <NavLink
          exact
          activeClassName="text-black"
          className="text-base font-semibold px-4"
          to="/leap/notifications"
        >
          New
        </NavLink>
        <NavLink
          activeClassName="text-black"
          className="text-base font-semibold px-4"
          to="/leap/notifications/archive"
        >
          Archive
        </NavLink>
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
      <Switch>
        <Route path="/leap/notifications" exact>
          <Inbox  />
        </Route>
        <Route path="/leap/notifications/archive" exact>
          <Inbox archived />
        </Route>
      </Switch>
    </div>
  );
};
