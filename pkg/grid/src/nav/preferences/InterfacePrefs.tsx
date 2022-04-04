import React from 'react';
import { Setting } from '../../components/Setting';
import {
  useBrowserNotifications,
  useBrowserSettings,
  useProtocolHandling,
  useSettingsState
} from '../../state/settings';
import { useBrowserId } from '../../state/local';

export function InterfacePrefs() {
  const settings = useBrowserSettings();
  const browserId = useBrowserId();
  const protocolHandling = useProtocolHandling(browserId);
  const browserNotifications = useBrowserNotifications(browserId);
  const secure = window.location.protocol === 'https:' || window.location.hostname === 'localhost';
  const linkHandlingAllowed = secure && 'registerProtocolHandler' in window.navigator;
  const setProtocolHandling = (setting: boolean) => {
    const newSettings = [{ browserId, protocolHandling: setting, browserNotifications }];
    if (!settings.includes(newSettings)) {
      useSettingsState
        .getState()
        .putEntry('browserSettings', 'settings', JSON.stringify(newSettings));
    }
  };
  const setBrowserNotifications = (setting: boolean) => {
    const newSettings = [{ browserId, browserNotifications: setting, protocolHandling }];
    if (!settings.includes(newSettings)) {
      useSettingsState
        .getState()
        .putEntry('browserSettings', 'settings', JSON.stringify(newSettings));
    }
  };

  const toggleProtoHandling = async () => {
    if (!protocolHandling && window?.navigator?.registerProtocolHandler) {
      try {
        window.navigator.registerProtocolHandler(
          'web+urbitgraph',
          '/apps/grid/perma?ext=%s',
          'Urbit Links'
        );
        setProtocolHandling(true);
      } catch (e) {
        console.error(e);
      }
    } else if (protocolHandling && window.navigator?.unregisterProtocolHandler) {
      try {
        window.navigator.unregisterProtocolHandler('web+urbitgraph', '/apps/grid/perma?ext=%s');
        setProtocolHandling(false);
      } catch (e) {
        console.error(e);
      }
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
      <h2 className="h3 mb-7">Interface Settings</h2>
      <div className="space-y-3">
        <Setting
          on={protocolHandling}
          toggle={toggleProtoHandling}
          name="Handle Urbit links"
          disabled={!linkHandlingAllowed}
        >
          <p>
            Automatically open urbit links when using this browser.
            {!linkHandlingAllowed && (
              <>
                ,{' '}
                <strong className="text-orange-500">
                  Unavailable with this browser/connection.
                </strong>
              </>
            )}
          </p>
        </Setting>
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
      </div>
    </>
  );
}
