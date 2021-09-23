import React from 'react';
import { Setting } from '../../components/Setting';
import { useProtocolHandling, setLocalState } from '../../state/local';

export function InterfacePrefs() {
  const protocolHandling = useProtocolHandling();
  const secure = window.location.protocol === 'https:';
  const linkHandlingAllowed = secure && !('registerProtocolHandler' in window.navigator);
  const toggleProtoHandling = async () => {
    if (!protocolHandling && window?.navigator?.registerProtocolHandler) {
      try {
        window.navigator.registerProtocolHandler(
          'web+urbitgraph',
          '/apps/grid/perma?ext=%s',
          'Urbit Links'
        );
        setLocalState((draft) => {
          draft.protocolHandling = true;
        });
      } catch (e) {
        console.error(e);
      }
    } else if (protocolHandling && window.navigator?.unregisterProtocolHandler) {
      try {
        window.navigator.unregisterProtocolHandler('web+urbitgraph', '/apps/grid/perma?ext=%s');
        setLocalState((draft) => {
          draft.protocolHandling = false;
        });
      } catch (e) {
        console.error(e);
      }
    }
  };

  return (
    <>
      <h2 className="h3 mb-7">Interface Settings</h2>
      <Setting
        on={protocolHandling}
        toggle={toggleProtoHandling}
        name="Handle Urbit links"
        disabled={!linkHandlingAllowed}
      >
        <p>
          Automatically open urbit links with this urbit
          {!linkHandlingAllowed && (
            <>
              , <strong className="text-orange-500">requires HTTPS</strong>
            </>
          )}
        </p>
      </Setting>
    </>
  );
}
