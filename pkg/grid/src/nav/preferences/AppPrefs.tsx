import React from 'react';
import { RouteComponentProps } from 'react-router-dom';
import { useCharge } from '../../state/docket';
import { usePike } from '../../state/kiln';
import { getAppName } from '../../state/util';
import SourceSyncer from '../../components/SourceSyncer';

export const AppPrefs = ({ match }: RouteComponentProps<{ desk: string }>) => {
  const { desk } = match.params;
  const charge = useCharge(desk);
  const appName = getAppName(charge);
  const pike = usePike(desk);
  const syncShip = pike?.sync?.ship;

  return (
    <SourceSyncer
      appName={appName}
      title={`${appName} Settings`}
      syncDesk={desk}
      syncShip={syncShip}
    />
  );
};
