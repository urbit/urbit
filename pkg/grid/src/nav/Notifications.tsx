import React, { useEffect } from 'react';
import { Link, NavLink, Route, Switch } from 'react-router-dom';
import { useLeapStore } from './Nav';
import { Button } from '../components/Button';
import { useHarkStore } from '../state/hark';
import { Inbox } from './notifications/Inbox';

export const Notifications = () => {
  const select = useLeapStore((s) => s.select);
  const markAllAsRead = () => {
    const { archiveAll } = useHarkStore.getState();
    archiveAll();
  };

  useEffect(() => {
    select('Notifications');

    function visibilitychange() {
      if (document.visibilityState === 'hidden') {
        useHarkStore.getState().opened();
      }
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
          <Inbox />
        </Route>
        <Route path="/leap/notifications/archive" exact>
          <Inbox archived />
        </Route>
      </Switch>
    </div>
  );
};
