import React, { useCallback } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import { Setting } from '../components/Setting';
import { ShipName } from '../components/ShipName';
import { useCharge } from '../state/docket';
import useKilnState, { useVat } from '../state/kiln';
import { getAppName } from '../state/util';

export const AppPrefs = ({ match }: RouteComponentProps<{ desk: string }>) => {
  const { desk } = match.params;
  const charge = useCharge(desk);
  const vat = useVat(desk);
  const tracking = !!vat?.arak.rail;
  const otasEnabled = !vat?.arak.rail?.paused;
  const otaSource = vat?.arak.rail?.ship;
  const toggleOTAs = useKilnState((s) => s.toggleOTAs);

  const toggleUpdates = useCallback((on: boolean) => toggleOTAs(desk, on), [desk, toggleOTAs]);

  return (
    <>
      <h2 className="h3 mb-7">{getAppName(charge)} Settings</h2>
      <div className="space-y-3">
        {tracking ? (
          <Setting on={otasEnabled} toggle={toggleUpdates} name="Automatic Updates">
            <p>Automatically download and apply updates to keep {getAppName(charge)} up to date.</p>
            {otaSource && (
              <p>
                OTA Source: <ShipName name={otaSource} className="font-semibold font-mono" />
              </p>
            )}
          </Setting>
        ) : (
          <h4 className="text-gray-500">No settings</h4>
        )}
      </div>
    </>
  );
};
