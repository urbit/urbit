import React, { useCallback, useEffect } from 'react';
import { Link, Route, RouteComponentProps, Switch, useRouteMatch } from 'react-router-dom';
import classNames from 'classnames';
import { useLeapStore } from './Nav';
import { NotificationPrefs } from './preferences/NotificationPrefs';
import { SystemUpdatePrefs } from './preferences/SystemUpdatePrefs';
import notificationsSVG from '../assets/notifications.svg';
import systemUpdatesSVG from '../assets/system-updates.svg';
import {InterfacePrefs} from './preferences/InterfacePrefs';

interface SystemPreferencesSectionProps extends RouteComponentProps<{ submenu: string }> {
  submenu: string;
  active: boolean;
  text: string;
  icon?: string;
}

function SystemPreferencesSection({
  match,
  submenu,
  active,
  icon,
  text
}: SystemPreferencesSectionProps) {
  const subMatch = useRouteMatch<{ submenu: string }>(`${match.url}/:submenu`);

  return (
    <li>
      <Link
        to={`${match.url}/${submenu}`}
        className={classNames(
          'flex items-center px-5 py-3 hover:text-black hover:bg-gray-100',
          active && 'text-black bg-gray-100'
        )}
      >
        {icon ? <img className="w-8 h-8 mr-3" src={icon} alt="" /> : null}
        {text}
      </Link>
    </li>
  );
}

export const SystemPreferences = (props: RouteComponentProps<{ submenu: string }>) => {
  const { match } = props;
  const select = useLeapStore((state) => state.select);
  const subMatch = useRouteMatch<{ submenu: string }>(`${match.url}/:submenu`);

  useEffect(() => {
    select('System Preferences');
  }, []);

  const matchSub = useCallback(
    (target: string) => {
      if (!subMatch && target === 'notifications') {
        return true;
      }

      return subMatch?.params.submenu === target;
    },
    [match, subMatch]
  );

  return (
    <div className="flex h-[600px] max-h-full">
      <aside className="flex-none min-w-60 border-r-2 border-gray-100">
        <div className="p-5">
          <input className="input h4 default-ring bg-gray-100" placeholder="Search Preferences" />
        </div>
        <nav className="border-b-2 border-gray-100">
          <ul className="font-semibold">
            <SystemPreferencesSection
              {...props}
              text="Notifications"
              icon={notificationsSVG}
              submenu="notifications"
              active={matchSub('notifications')}
            />
            <SystemPreferencesSection
              {...props}
              text="System Updates"
              icon={systemUpdatesSVG}
              submenu="system-updates"
              active={matchSub('system-updates')}
            />
            <SystemPreferencesSection
              {...props}
              text="Interface Settings"
              icon={systemUpdatesSVG}
              submenu="interface"
              active={matchSub('interface')}
            />
          </ul>
        </nav>
      </aside>
      <section className="flex-1 px-5 py-7 text-black">
        <Switch>
          <Route path={`${match.url}/system-updates`} component={SystemUpdatePrefs} />
          <Route path={`${match.url}/interface`} component={InterfacePrefs} />
          <Route path={[`${match.url}/notifications`, match.url]} component={NotificationPrefs} />
        </Switch>
      </section>
    </div>
  );
};
