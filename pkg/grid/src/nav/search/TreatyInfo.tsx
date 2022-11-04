import React, { useEffect } from 'react';
import { useParams } from 'react-router-dom';
import { AppInfo } from '../../components/AppInfo';
import { Spinner } from '../../components/Spinner';
import useDocketState, { useCharge, useTreaty } from '../../state/docket';
import { usePike } from '../../state/kiln';
import { getAppName } from '../../state/util';
import { useLeapStore } from '../Nav';

export const TreatyInfo = () => {
  const select = useLeapStore((state) => state.select);
  const { host, desk } = useParams<{ host: string; desk: string }>();
  const treaty = useTreaty(host, desk);
  const pike = usePike(desk);
  const charge = useCharge(desk);
  const name = getAppName(treaty);

  useEffect(() => {
    if (!charge) {
      useDocketState.getState().requestTreaty(host, desk);
    }
  }, [host, desk]);

  useEffect(() => {
    select(<>{name}</>);
    useLeapStore.setState({ matches: [] });
  }, [name]);

  if (!treaty) {
    // TODO: maybe replace spinner with skeletons
    return (
      <div className="dialog-inner-container flex justify-center text-black">
        <Spinner className="w-10 h-10" />
      </div>
    );
  }
  return <AppInfo className="dialog-inner-container" docket={charge || treaty} pike={pike} />;
};
