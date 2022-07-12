import React, { PropsWithChildren, useCallback } from 'react';
import { Link, Route, RouteComponentProps, Switch, useRouteMatch } from 'react-router-dom';
import { ErrorBoundary } from 'react-error-boundary';
import classNames from 'classnames';
import { NotificationPrefs } from './preferences/NotificationPrefs';
import { SystemUpdatePrefs } from './preferences/SystemUpdatePrefs';
import { InterfacePrefs } from './preferences/InterfacePrefs';
import { SecurityPrefs } from './preferences/SecurityPrefs';
import { useCharges } from '../state/docket';
import { AppPrefs } from './preferences/AppPrefs';
import { DocketImage } from '../components/DocketImage';
import { ErrorAlert } from '../components/ErrorAlert';
import { useMedia } from '../logic/useMedia';
import { LeftArrow } from '../components/icons/LeftArrow';
import { System } from '../components/icons/System';
import { Interface } from '../components/icons/Interface';
import { Notifications } from '../components/icons/Notifications';
import { Lock } from '../components/icons/Lock';
import { getAppName } from '../state/util';
import { Help } from './Help';

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
  const charges = useCharges();
  const filteredCharges = Object.values(charges).filter((charge) => charge.desk !== window.desk);
  const isMobile = useMedia('(max-width: 639px)');
  const settingsPath = isMobile ? `${match.url}/:submenu` : '/';

  const matchSub = useCallback(
    (target: string, desk?: string) => {
      if (isMobile) {
        return false;
      }

      if (!subMatch && target === 'notifications') {
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
              {/* TODO: Replace this h3 with the search box. */}
              <h2 className="px-2 mb-4 h3">System Preferences</h2>
              <span className="text-gray-400 font-semibold pt-1 pl-2 pb-3 text-xs">Landscape</span>
              <ul className="space-y-1">
                <SystemPreferencesSection
                  url={subUrl('system-updates')}
                  active={matchSub('system-updates')}
                >
                  <System className="w-8 h-8 mr-3 bg-gray-100 rounded-md" />
                  About System
                </SystemPreferencesSection>
                <SystemPreferencesSection url={subUrl('help')} active={matchSub('help')}>
                  <Lock className="w-8 h-8 mr-3 bg-gray-100 rounded-md" />
                  Help and Support
                </SystemPreferencesSection>
                <SystemPreferencesSection url={subUrl('security')} active={matchSub('security')}>
                  <Lock className="w-8 h-8 mr-3 bg-gray-100 rounded-md" />
                  Log Out...
                </SystemPreferencesSection>
              </ul>
            </nav>
            <nav className="px-2 sm:px-6 flex flex-col">
              <span className="text-gray-400 font-semibold pt-5 pl-2 pb-3 text-xs">Settings</span>
              <ul className="space-y-1">
                <SystemPreferencesSection
                  url={subUrl('notifications')}
                  active={matchSub('notifications')}
                >
                  <Notifications className="w-8 h-8 mr-3 bg-gray-100 rounded-md" />
                  Notifications
                </SystemPreferencesSection>
                <SystemPreferencesSection url={subUrl('interface')} active={matchSub('interface')}>
                  <Interface className="w-8 h-8 mr-3 bg-gray-100 rounded-md" />
                  Interface Settings
                </SystemPreferencesSection>
              </ul>
            </nav>
            <nav className="px-2 sm:px-6 flex flex-col">
              <span className="text-gray-400 font-semibold pt-5 pl-2 pb-3 text-xs">
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
          <section className="flex-1 flex flex-col min-h-[60vh] p-4 sm:p-8 text-black bg-gray-50">
            <Switch>
              <Route path={`${match.url}/apps/:desk`} component={AppPrefs} />
              <Route path={`${match.url}/system-updates`} component={SystemUpdatePrefs} />
              <Route path={`${match.url}/help`} component={Help} />
              <Route path={`${match.url}/interface`} component={InterfacePrefs} />
              <Route path={`${match.url}/security`} component={SecurityPrefs} />
              <Route
                path={[`${match.url}/notifications`, match.url]}
                component={NotificationPrefs}
              />
            </Switch>
            <Link
              to={match.url}
              className="inline-flex items-center pt-4 mt-auto text-gray-400 sm:hidden sm:none h4"
            >
              <LeftArrow className="w-3 h-3 mr-2" /> Back
            </Link>
          </section>
        </Route>
      </div>
    </ErrorBoundary>
  );
};
