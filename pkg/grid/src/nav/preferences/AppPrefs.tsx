import React, { useCallback } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import { Setting } from '../../components/Setting';
import { ShipName } from '../../components/ShipName';
import { useCharge } from '../../state/docket';
import useKilnState, { useVat } from '../../state/kiln';

export const AppPrefs = ({ match }: RouteComponentProps<{ desk: string }>) => {
  const { desk } = match.params;
  const charge = useCharge(desk);
  const vat = useVat(desk);
  const otasEnabled = !vat?.arak.paused;
  const otaSource = vat?.arak.ship;
  const toggleOTAs = useKilnState((s) => s.toggleOTAs);

  const toggleUpdates = useCallback((on: boolean) => toggleOTAs(desk, on), [desk, toggleOTAs]);

  return (
    <>
      <h2 className="h3 mb-7">{charge?.title} Settings</h2>
      <div className="space-y-3">
        <Setting on={!!otasEnabled} toggle={toggleUpdates} name="Automatic Updates">
          <p>Automatically download and apply updates to keep {charge?.title} up to date.</p>
          {otaSource && (
            <p>
              OTA Source: <ShipName name={otaSource} className="font-semibold font-mono" />
            </p>
          )}
        </Setting>
      </div>
    </>
  );
};
