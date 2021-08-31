import produce from 'immer';
import create from 'zustand';
import React, { useEffect } from 'react';
import { persist } from 'zustand/middleware';
import { take } from 'lodash-es';
import { Provider } from '@urbit/api';
import { MatchItem, useLeapStore } from '../Nav';
import { providerMatch } from './Providers';
import { AppList } from '../../components/AppList';
import { ProviderList } from '../../components/ProviderList';
import { AppLink } from '../../components/AppLink';
import { ShipName } from '../../components/ShipName';
import { ProviderLink } from '../../components/ProviderLink';
import { DocketWithDesk, useCharges } from '../../state/docket';
import { getAppHref } from '../../state/util';

export interface RecentsStore {
  recentApps: DocketWithDesk[];
  recentDevs: Provider[];
  addRecentApp: (app: DocketWithDesk) => void;
  addRecentDev: (dev: Provider) => void;
}

export const useRecentsStore = create<RecentsStore>(
  persist(
    (set) => ({
      recentApps: [],
      recentDevs: [],
      addRecentApp: (app) => {
        set(
          produce((draft: RecentsStore) => {
            const hasApp = draft.recentApps.find((a) => a.desk === app.desk);
            if (!hasApp) {
              draft.recentApps.unshift(app);
            }

            draft.recentApps = take(draft.recentApps, 3);
          })
        );
      },
      addRecentDev: (dev) => {
        set(
          produce((draft: RecentsStore) => {
            const hasDev = draft.recentDevs.find((p) => p.shipName === dev.shipName);
            if (!hasDev) {
              draft.recentDevs.unshift(dev);
            }

            draft.recentDevs = take(draft.recentDevs, 3);
          })
        );
      }
    }),
    {
      whitelist: ['recentApps', 'recentDevs'],
      name: 'recents-store'
    }
  )
);

window.recents = useRecentsStore.getState;

export function addRecentDev(dev: Provider) {
  return useRecentsStore.getState().addRecentDev(dev);
}

export function addRecentApp(app: DocketWithDesk) {
  return useRecentsStore.getState().addRecentApp(app);
}

export const Home = () => {
  const selectedMatch = useLeapStore((state) => state.selectedMatch);
  const { recentApps, recentDevs } = useRecentsStore();
  const charges = useCharges();
  const groups = charges?.groups;
  const zod = { shipName: '~zod' };

  useEffect(() => {
    const apps = recentApps.map((app) => ({
      url: getAppHref(app.href),
      openInNewTab: true,
      value: app.desk,
      display: app.title
    }));
    const devs = recentDevs.map(providerMatch);

    useLeapStore.setState({
      matches: ([] as MatchItem[]).concat(apps, devs)
    });
  }, [recentApps, recentDevs]);

  return (
    <div className="h-full p-4 md:p-8 font-semibold leading-tight text-black overflow-y-auto">
      <h2 id="recent-apps" className="mb-6 h4 text-gray-500">
        Recent Apps
      </h2>
      {recentApps.length === 0 && (
        <div className="min-h-[150px] p-6 rounded-xl bg-gray-100">
          <p className="mb-4">Apps you use will be listed here, in the order you used them.</p>
          <p className="mb-6">You can click/tap/keyboard on a listed app to open it.</p>
          {groups && (
            <AppLink
              app={groups}
              size="small"
              onClick={() => addRecentApp({ ...groups, desk: 'groups' })}
            />
          )}
        </div>
      )}
      {recentApps.length > 0 && (
        <AppList
          apps={recentApps}
          labelledBy="recent-apps"
          matchAgainst={selectedMatch}
          size="small"
        />
      )}
      <hr className="-mx-4 my-6 md:-mx-8 md:my-9" />
      <h2 id="recent-devs" className="mb-6 h4 text-gray-500">
        Recent Developers
      </h2>
      {recentDevs.length === 0 && (
        <div className="min-h-[150px] p-6 rounded-xl bg-gray-100">
          <p className="mb-4">Urbit app developers you search for will be listed here.</p>
          <p className="mb-6">
            Try out app discovery by visiting <ShipName name="~zod" /> below.
          </p>
          <ProviderLink provider={zod} small onClick={() => addRecentDev(zod)} />
        </div>
      )}
      {recentDevs.length > 0 && (
        <ProviderList
          providers={recentDevs}
          labelledBy="recent-devs"
          matchAgainst={selectedMatch}
          small
        />
      )}
    </div>
  );
};
