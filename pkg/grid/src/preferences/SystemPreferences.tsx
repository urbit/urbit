import React, { PropsWithChildren, useCallback } from 'react';
import { Link, Route, RouteComponentProps, Switch, useRouteMatch } from 'react-router-dom';
import { ErrorBoundary } from 'react-error-boundary';
import classNames from 'classnames';
import { NotificationPrefs } from './NotificationPrefs';
import { AboutSystem } from './about-system/AboutSystem';
import { InterfacePrefs } from './InterfacePrefs';
import { SecurityPrefs } from './SecurityPrefs';
import { AppearancePrefs } from './ApperancePrefs';
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
import SearchSystemPreferences from './SearchSystemPrefences';

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
              <SearchSystemPreferences subUrl={subUrl} />
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
              <Route path={`${match.url}/appearance`} component={AppearancePrefs} />
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
