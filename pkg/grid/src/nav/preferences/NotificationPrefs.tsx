import { setMentions } from '@urbit/api';
import React from 'react';
import { Setting } from '../../components/Setting';
import { pokeOptimisticallyN } from '../../state/base';
import { HarkState, reduceGraph, useHarkStore } from '../../state/hark';
import { useBrowserId } from '../../state/local';
import {
  useSettingsState,
  useBrowserNotifications,
  useBrowserSettings,
  useProtocolHandling
} from '../../state/settings';

const selMentions = (s: HarkState) => s.notificationsGraphConfig.mentions;
async function toggleMentions() {
  const state = useHarkStore.getState();
  await pokeOptimisticallyN(useHarkStore, setMentions(!selMentions(state)), reduceGraph);
}

export const NotificationPrefs = () => {
  const mentions = useHarkStore(selMentions);
  const settings = useBrowserSettings();
  const browserId = useBrowserId();
  const browserNotifications = useBrowserNotifications(browserId);
  const protocolHandling = useProtocolHandling(browserId);
  const secure = window.location.protocol === 'https:' || window.location.hostname === 'localhost';

  const setBrowserNotifications = (setting: boolean) => {
    const newSettings = [{ browserId, browserNotifications: setting, protocolHandling }];
    if (!settings.includes(newSettings)) {
      useSettingsState
        .getState()
        .putEntry('browserSettings', 'settings', JSON.stringify(newSettings));
    }
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
      <h2 className="h3 mb-7">Notifications</h2>
      <div className="space-y-3">
        <Setting
          on={browserNotifications}
          toggle={toggleNotifications}
          name="Show desktop notifications"
          disabled={!secure}
        >
          <p>
            Show desktop notifications in this browser.
            {!secure && (
              <>
                , <strong className="text-orange-500">requires HTTPS</strong>
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
