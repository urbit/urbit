import React, { useEffect } from 'react';
import { useParams } from 'react-router-dom';
import { AppInfo } from '../../components/AppInfo';
import { ShipName } from '../../components/ShipName';
import useDocketState, { useCharge, useTreaty } from '../../state/docket';
import { useVat } from '../../state/kiln';
import { useLeapStore } from '../Nav';

export const TreatyInfo = () => {
  const select = useLeapStore((state) => state.select);
  const { ship, host, desk } = useParams<{ ship: string; host: string; desk: string }>();
  const treaty = useTreaty(host, desk);
  const vat = useVat(desk);
  const charge = useCharge(desk);

  useEffect(() => {
    if(!charge) {
      useDocketState.getState().requestTreaty(host, desk);
    }
  }, [host, desk]);

  useEffect(() => {
    select(
      <>
        {treaty?.title}
      </>
    );
  }, [treaty?.title]);

  if (!treaty) {
    // TODO: maybe replace spinner with skeletons
    return (
      <div className="dialog-inner-container text-black">
        <span>Loading...</span>
      </div>
    );
  }
  return <AppInfo className="dialog-inner-container" docket={charge || treaty} vat={vat} />;
};
