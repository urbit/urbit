// import { setMentions } from '@urbit/api';
import React from 'react';
import { DocketImage } from '../components/DocketImage';
import { Setting } from '../components/Setting';
import { Toggle } from '../components/Toggle';
// import { pokeOptimisticallyN } from '../state/base';
import { ChargeWithDesk, useCharges } from '../state/docket';
// import { HarkState, reduceGraph, useHarkStore } from '../state/hark';
import { useBrowserId } from '../state/local';
import {
  useSettingsState,
  useBrowserNotifications,
  useBrowserSettings,
  SettingsState,
  setBrowserSetting
} from '../state/settings';
import { getAppName } from '../state/util';

const selDnd = (s: SettingsState) => s.display.doNotDisturb;
async function toggleDnd() {
  const state = useSettingsState.getState();
  const curr = selDnd(state);
  await state.putEntry('display', 'doNotDisturb', !curr);
}
// TODO: Find out where mentions toggle should go.
// const selMentions = (s: HarkState) => s.notificationsGraphConfig.mentions;
// async function toggleMentions() {
// const state = useHarkStore.getState();
// await pokeOptimisticallyN(useHarkStore, setMentions(!selMentions(state)), reduceGraph);
// }

interface AppNotificationRowProps {
  charge: ChargeWithDesk;
  active: boolean;
  receivedToday: number;
}

const AppNotificationRow = ({ charge, active, receivedToday }: AppNotificationRowProps) => (
  <div className="flex items-center justify-between">
    <div className="flex items-center">
      <DocketImage size="small" className="mr-3" {...charge} />
      <div className="flex flex-col">
        <span className="text-gray-800 font-semibold leading-4">{getAppName(charge)}</span>
        <div className="flex font-semibold text-sm space-x-2">
          {active ? (
            <span className="text-blue-500">Notifications Active</span>
          ) : (
            <span className="text-gray-400">Notifications Paused</span>
          )}
          <span className="text-gray-200">{receivedToday} received today</span>
        </div>
      </div>
    </div>
    <Toggle
      pressed={active}
      onPressedChange={() => console.log(`Toggle notifications for ${charge.desk}`)}
      className="flex-none self-start text-blue-400"
    />
  </div>
);

export const NotificationPrefs = () => {
  const doNotDisturb = useSettingsState(selDnd);
  const charges = useCharges();
  const filteredCharges = Object.values(charges).filter((charge) => charge.desk !== window.desk);
  // const mentions = useHarkStore(selMentions);
  const settings = useBrowserSettings();
  const browserId = useBrowserId();
  const browserNotifications = useBrowserNotifications(browserId);
  const secure = window.location.protocol === 'https:' || window.location.hostname === 'localhost';
  const notificationsAllowed = secure && 'Notification' in window;

  const setBrowserNotifications = (setting: boolean) => {
    const newSettings = setBrowserSetting(settings, { browserNotifications: setting }, browserId);
    useSettingsState
      .getState()
      .putEntry('browserSettings', 'settings', JSON.stringify(newSettings));
  };

  const toggleNotifications = async () => {
    if (!browserNotifications) {
      Notification.requestPermission();
      setBrowserNotifications(true);
    } else {
      setBrowserNotifications(false);
    }
  };

  return (
    <>
      <div className="space-y-3">
        <div className="inner-section space-y-8">
          <span className="text-lg font-bold leading-6">Landscape Notifications</span>
          <Setting on={doNotDisturb} toggle={toggleDnd} name="Mute Landscape Notifications">
            <p className="text-gray-600 leading-5 pt-1">
              Enabling this setting will render all notifications from all applications being muted
              at the Landscape level, which mutes your urbit’s notifications on your host OS as
              well.
            </p>
            <p className="text-gray-600 leading-5 pt-3">
              Apps will continue to aggregate and post notifications to Landscape’s notification
              feed, but new app activity will not be visually signaled.
            </p>
          </Setting>
          <Setting
            on={browserNotifications}
            toggle={toggleNotifications}
            disabled={!notificationsAllowed}
            name="Enable OS Notifications"
          >
            <p className="text-gray-600 leading-5 pt-1">
              Turn on to receive notifications from your host operating system.
            </p>
          </Setting>
        </div>
        <div className="inner-section space-y-8">
          <span className="text-lg font-semibold leading-6">App Notifications</span>
          <div className="flex flex-col space-y-2">
            {filteredCharges.map((charge, index) => (
              <AppNotificationRow
                key={`${charge.desk}-notification-row`}
                charge={charge}
                // TODO: Where are we getting this from?
                active={index % 2 === 0}
                receivedToday={index + 1}
              />
            ))}
          </div>
        </div>
      </div>
    </>
  );
};
