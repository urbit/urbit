import React from 'react';
import { Setting } from '../components/Setting';
import {
  setBrowserSetting,
  useBrowserSettings,
  useProtocolHandling,
  useSettingsState
} from '../state/settings';
import { useBrowserId } from '../state/local';

export function InterfacePrefs() {
  const settings = useBrowserSettings();
  const browserId = useBrowserId();
  const protocolHandling = useProtocolHandling(browserId);
  const secure = window.location.protocol === 'https:' || window.location.hostname === 'localhost';
  const linkHandlingAllowed = secure && 'registerProtocolHandler' in window.navigator;
  const setProtocolHandling = (setting: boolean) => {
    const newSettings = setBrowserSetting(settings, { protocolHandling: setting }, browserId);
    useSettingsState
      .getState()
      .putEntry('browserSettings', 'settings', JSON.stringify(newSettings));
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

  return (
    <>
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
                <strong className="text-orange-500">
                  Unavailable with this browser/connection.
                </strong>
              </>
            )}
          </p>
        </Setting>
      </div>
    </>
  );
}
