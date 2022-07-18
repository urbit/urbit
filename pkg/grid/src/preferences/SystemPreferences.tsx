import React, { ChangeEvent, PropsWithChildren, useCallback, useEffect, useState } from 'react';
import { Link, Route, RouteComponentProps, Switch, useRouteMatch } from 'react-router-dom';
import { ErrorBoundary } from 'react-error-boundary';
import classNames from 'classnames';
import fuzzy from 'fuzzy';
import { NotificationPrefs } from './NotificationPrefs';
import { AboutSystem } from './about-system/AboutSystem';
import { InterfacePrefs } from './InterfacePrefs';
import { SecurityPrefs } from './SecurityPrefs';
import { useCharges } from '../state/docket';
import { AppPrefs } from './AppPrefs';
import { DocketImage } from '../components/DocketImage';
import { ErrorAlert } from '../components/ErrorAlert';
import { useMedia } from '../logic/useMedia';
import { LeftArrow } from '../components/icons/LeftArrow';
import { Interface } from '../components/icons/Interface';
import { getAppName } from '../state/util';
import { Help } from '../nav/Help';
import TlonIcon from '../components/icons/TlonIcon';
import HelpIcon from '../components/icons/HelpIcon';
import LogoutIcon from '../components/icons/LogoutIcon';
import BellIcon from '../components/icons/BellIcon';
import BurstIcon from '../components/icons/BurstIcon';
import PencilIcon from '../components/icons/PencilIcon';
import ForwardSlashIcon from '../components/icons/ForwardSlashIcon';
import { useSystemUpdate } from '../logic/useSystemUpdate';
import { Bullet } from '../components/icons/Bullet';
import MagnifyingGlassIcon from '../components/icons/MagnifyingGlassIcon';

interface SystemPreferencesSectionProps {
  url: string;
  active: boolean;
}

function SystemPreferencesSection({
  url,
  active,
  children
}: PropsWithChildren<SystemPreferencesSectionProps>) {
  return (
    <li>
      <Link
        to={url}
        className={classNames(
          'flex items-center px-2 py-2 hover:text-black hover:bg-gray-50 rounded-lg',
          active && 'text-black bg-gray-50'
        )}
      >
        {children}
      </Link>
    </li>
  );
}

const navOptions: { route: string; title: string; icon: React.ReactElement }[] = [
  {
    route: 'help',
    title: 'Help and Support',
    icon: <HelpIcon className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'interface',
    title: 'Interface Settings',
    icon: <Interface className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'notifications',
    title: 'Notifications',
    icon: <BellIcon className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'appearance',
    title: 'Appearance',
    icon: <PencilIcon className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'shortcuts',
    title: 'Shortcuts',
    icon: <ForwardSlashIcon className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'privacy',
    title: 'Attention & Privacy',
    icon: <BurstIcon className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'security',
    title: 'Log Out...',
    icon: <LogoutIcon className="w-4 h-4 text-gray-600" />
  },
  {
    route: 'system-updates',
    title: 'About System',
    icon: <TlonIcon className="w-4 h-4 text-gray-600" />
  }
];

export const SystemPreferences = (props: RouteComponentProps<{ submenu: string }>) => {
  const { match, history } = props;
  const subMatch = useRouteMatch<{ submenu: string; desk?: string }>(
    `${match.url}/:submenu/:desk?`
  );
  const { systemBlocked } = useSystemUpdate();
  const charges = useCharges();
  const filteredCharges = Object.values(charges).filter((charge) => charge.desk !== window.desk);
  const isMobile = useMedia('(max-width: 639px)');
  const settingsPath = isMobile ? `${match.url}/:submenu` : '/';
  const [searchInput, setSearchInput] = useState('');
  const [matchingNavOptions, setMatchingNavOptions] = useState<string[]>([]);

  const handleChange = (e: ChangeEvent<HTMLInputElement>) => {
    const input = e.target as HTMLInputElement;
    const value = input.value.trim();

    setSearchInput(value);
  };

  useEffect(() => {
    const results = fuzzy.filter(searchInput, navOptions, { extract: (obj) => obj.title });
    const matches = results.map((el) => el.string);
    setMatchingNavOptions(matches);
  }, [searchInput]);

  const matchSub = useCallback(
    (target: string, desk?: string) => {
      if (isMobile) {
        return false;
      }

      if (!subMatch && target === 'system-updates') {
        return true;
      }

      if (desk && subMatch?.params.desk !== desk) {
        return false;
      }

      return subMatch?.params.submenu === target;
    },
    [match, subMatch]
  );

  const subUrl = useCallback((submenu: string) => `${match.url}/${submenu}`, [match]);

  return (
    <ErrorBoundary
      FallbackComponent={ErrorAlert}
      onReset={() => history.push('/leap/system-preferences')}
    >
      <div className="h-full overflow-y-auto sm:flex bg-gray-50">
        <Route exact={isMobile} path={match.url}>
          <aside className="self-start flex-none w-full py-4 font-semibold text-black border-r-2 sm:w-auto min-w-60 sm:py-8 sm:text-gray-600 border-gray-50 bg-white">
            <nav className="px-2 sm:px-6 flex flex-col">
              <label className="relative flex items-center">
                <span className="sr-only">Search Prefences</span>
                <span className="absolute h-8 w-8 text-gray-400 flex items-center pl-2 inset-y-1 left-0">
                  <MagnifyingGlassIcon />
                </span>
                <input
                  className="input bg-gray-50 pl-8 placeholder:font-semibold mb-5 h-10"
                  placeholder="Search Preferences"
                  value={searchInput}
                  onChange={handleChange}
                />
              </label>
              <div className="relative">
                {matchingNavOptions.length > 0 && searchInput !== '' ? (
                  <div className="absolute -top-3 flex flex-col bg-white space-y-2 rounded-2xl shadow-md w-full py-3">
                    {matchingNavOptions.map((opt) => {
                      const matchingNavOption = navOptions.find((navOpt) => navOpt.title === opt);
                      if (matchingNavOption !== undefined) {
                        return (
                          <Link
                            className="flex px-2 py-3 items-center space-x-2 hover:text-black hover:bg-gray-50"
                            to={subUrl(matchingNavOption.route)}
                          >
                            {matchingNavOption.icon}
                            <span className="text-gray-900">{matchingNavOption?.title}</span>
                          </Link>
                        );
                      }
                      return null;
                    })}
                  </div>
                ) : null}
              </div>
              <span className="text-gray-400 font-semibold pt-1 pl-2 pb-3 text-sm">Landscape</span>
              <ul className="space-y-1">
                <SystemPreferencesSection
                  url={subUrl('system-updates')}
                  active={matchSub('system-updates')}
                >
                  <TlonIcon className="w-6 h-6 mr-3 rounded-md text-gray-600" />
                  About System
                  {systemBlocked && <Bullet className="h-5 w-5 ml-auto text-orange-500" />}
                </SystemPreferencesSection>
                <SystemPreferencesSection url={subUrl('help')} active={matchSub('help')}>
                  <HelpIcon className="w-6 h-6 mr-3 rounded-md text-gray-600" />
                  Help and Support
                </SystemPreferencesSection>
                <SystemPreferencesSection url={subUrl('security')} active={matchSub('security')}>
                  <LogoutIcon className="w-6 h-6 mr-3 rounded-md text-gray-600" />
                  Log Out...
                </SystemPreferencesSection>
              </ul>
            </nav>
            <nav className="px-2 sm:px-6 flex flex-col">
              <span className="text-gray-400 font-semibold pt-5 pl-2 pb-3 text-sm">Settings</span>
              <ul className="space-y-1">
                <SystemPreferencesSection
                  url={subUrl('notifications')}
                  active={matchSub('notifications')}
                >
                  <BellIcon className="w-6 h-6 mr-3 rounded-md text-gray-600" />
                  Notifications
                </SystemPreferencesSection>
                <SystemPreferencesSection url={subUrl('privacy')} active={matchSub('privacy')}>
                  <BurstIcon className="w-6 h-6 mr-3 rounded-md text-gray-600" />
                  Attention & Privacy
                </SystemPreferencesSection>
                <SystemPreferencesSection
                  url={subUrl('appearance')}
                  active={matchSub('appearance')}
                >
                  <PencilIcon className="w-6 h-6 mr-3 rounded-md text-gray-600" />
                  Appearance
                </SystemPreferencesSection>
                <SystemPreferencesSection url={subUrl('shortcuts')} active={matchSub('shortcuts')}>
                  <ForwardSlashIcon className="w-6 h-6 mr-3 rounded-md text-gray-600" />
                  Shortcuts
                </SystemPreferencesSection>
                <SystemPreferencesSection url={subUrl('interface')} active={matchSub('interface')}>
                  <Interface className="w-8 h-8 mr-3 bg-gray-100 rounded-md" />
                  Interface Settings
                </SystemPreferencesSection>
              </ul>
            </nav>
            <nav className="px-2 sm:px-6 flex flex-col">
              <span className="text-gray-400 font-semibold pt-5 pl-2 pb-3 text-sm">
                Installed App Settings
              </span>
              <ul className="space-y-1">
                {filteredCharges.map((charge) => (
                  <SystemPreferencesSection
                    key={charge.desk}
                    url={subUrl(`apps/${charge.desk}`)}
                    active={matchSub('apps', charge.desk)}
                  >
                    <DocketImage size="small" className="mr-3" {...charge} />
                    {getAppName(charge)}
                  </SystemPreferencesSection>
                ))}
              </ul>
            </nav>
          </aside>
        </Route>
        <Route path={settingsPath}>
          <section className="flex-1 flex flex-col min-h-[60vh] p-4 sm:p-8 text-gray-800 bg-gray-50">
            <Switch>
              <Route path={`${match.url}/apps/:desk`} component={AppPrefs} />
              <Route path={`${match.url}/help`} component={Help} />
              <Route path={`${match.url}/interface`} component={InterfacePrefs} />
              <Route path={`${match.url}/notifications`} component={NotificationPrefs} />
              <Route path={[`${match.url}/system-updates`, match.url]} component={AboutSystem} />
            </Switch>
            <Link
              to={match.url}
              className="inline-flex items-center pt-4 mt-auto text-gray-400 sm:hidden sm:none h4"
            >
              <LeftArrow className="w-3 h-3 mr-2" /> Back
            </Link>
          </section>
        </Route>
        <Route path={`${match.url}/security`} component={SecurityPrefs} />
      </div>
    </ErrorBoundary>
  );
};
