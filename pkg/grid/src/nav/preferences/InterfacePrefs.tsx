import React from 'react';
import { Setting } from '../../components/Setting';
import { ShipName } from '../../components/ShipName';
import { useProtocolHandling, setLocalState } from '../../state/local';

export function InterfacePrefs() {
  const protocolHandling = useProtocolHandling();
  const toggleProtoHandling = async () => {
    if (!protocolHandling && window?.navigator?.registerProtocolHandler) {
      try {
        window.navigator.registerProtocolHandler('web+urbitgraph', '/apps/grid/perma?ext=%s', 'Urbit Links');
        setLocalState((s) => {
          s.protocolHandling = true;
        });
      } catch (e) {
        console.error(e);
      }
    } else if (protocolHandling && window.navigator?.unregisterProtocolHandler) {
      try {
        window.navigator.unregisterProtocolHandler('web+urbitgraph', '/apps/grid/perma?ext=%s');
        setLocalState((s) => {
          s.protocolHandling = false;
        });
      } catch (e) {
        console.error(e);
      }
    }
  };

  return (
    <>
      <h2 className="h3 mb-7">Interface Settings</h2>
      <Setting on={protocolHandling} toggle={toggleProtoHandling} name="Handle Urbit links">
        <p>Automatically open urbit links with this urbit</p>
      </Setting>

      <div className="space-y-3"> </div>
    </>
  );
}
