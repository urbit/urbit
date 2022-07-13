import { setMentions } from '@urbit/api';
import React from 'react';
import { Setting } from '../components/Setting';
import { pokeOptimisticallyN } from '../state/base';
import { HarkState, reduceGraph, useHarkStore } from '../state/hark';
import { useBrowserId } from '../state/local';
import {
  useSettingsState,
  useBrowserNotifications,
  useBrowserSettings,
  SettingsState,
  setBrowserSetting
} from '../state/settings';

const selDnd = (s: SettingsState) => s.display.doNotDisturb;
async function toggleDnd() {
  const state = useSettingsState.getState();
  const curr = selDnd(state);
  await state.putEntry('display', 'doNotDisturb', !curr);
}

const selMentions = (s: HarkState) => s.notificationsGraphConfig.mentions;
async function toggleMentions() {
  const state = useHarkStore.getState();
  await pokeOptimisticallyN(useHarkStore, setMentions(!selMentions(state)), reduceGraph);
}

export const NotificationPrefs = () => {
  const doNotDisturb = useSettingsState(selDnd);
  const mentions = useHarkStore(selMentions);
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
        <Setting on={doNotDisturb} toggle={toggleDnd} name="Do Not Disturb">
          <p>
            Blocks Urbit notifications in Landscape from appearing as badges and prevents browser
            notifications if enabled.
          </p>
        </Setting>
        <Setting
          on={browserNotifications}
          toggle={toggleNotifications}
          name="Show Desktop Notifications"
          disabled={!notificationsAllowed}
        >
          <p>
            Show desktop notifications in this browser.
            {!secure && (
              <>
                <strong className="text-orange-500">
                  Unavailable with this browser/connection.
                </strong>
              </>
            )}
          </p>
        </Setting>
        <Setting on={mentions} toggle={toggleMentions} name="Mentions">
          <p>Notify me if someone mentions my @p in a channel I&apos;ve joined</p>
        </Setting>
      </div>
    </>
  );
};
