import React, { useEffect } from 'react';
import { ErrorBoundary } from 'react-error-boundary';
import { Link, NavLink, Route, RouteComponentProps, Switch } from 'react-router-dom';
import { Button } from '../components/Button';
import { ErrorAlert } from '../components/ErrorAlert';
import { useHarkStore } from '../state/hark';
import { Inbox } from './notifications/Inbox';

export const Notifications = ({ history }: RouteComponentProps) => {
  const markAllAsRead = () => {
    const { archiveAll } = useHarkStore.getState();
    archiveAll();
  };

  useEffect(() => {
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
    <ErrorBoundary
      FallbackComponent={ErrorAlert}
      onReset={() => history.push('/leap/notifications')}
    >
      <div className="grid grid-rows-[1fr,auto] sm:grid-rows-[auto,1fr] h-full p-4 md:p-8 overflow-y-auto">
        <header className="order-last sm:order-none flex flex-wrap justify-start items-center w-full gap-2 mt-8 sm:mt-0 sm:mb-8">
          <NavLink
            exact
            activeClassName="text-black"
            className="flex-none font-semibold px-4"
            to="/leap/notifications"
          >
            New
          </NavLink>
          <NavLink
            activeClassName="text-black"
            className="flex-none font-semibold px-4"
            to="/leap/notifications/archive"
          >
            Archive
          </NavLink>
          <span className="flex-none inline-block sm:hidden w-full flex-shrink-0" />
          <Button
            onClick={markAllAsRead}
            variant="secondary"
            className="flex-auto sm:flex-none py-1.5 px-2 sm:px-6 text-sm sm:text-base rounded-full"
          >
            Archive All
          </Button>
          <Button
            as={Link}
            variant="secondary"
            to="/leap/system-preferences/notifications"
            className="flex-auto sm:flex-none py-1.5 px-3 sm:px-6 text-sm sm:text-base rounded-full"
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
    </ErrorBoundary>
  );
};
