import React from 'react';
import { RouteComponentProps } from 'react-router-dom';
import SourceSetter from '../../components/SourceSetter';
import { useCharge } from '../../state/docket';
import useKilnState, { usePike } from '../../state/kiln';
import { getAppName } from '../../state/util';

export const AppPrefs = ({ match }: RouteComponentProps<{ desk: string }>) => {
  const { desk } = match.params;
  const charge = useCharge(desk);
  const appName = getAppName(charge);
  const pike = usePike(desk);
  const srcShip = pike?.sync?.ship;
  const { toggleSync } = useKilnState();

  return (
    <SourceSetter
      appName={appName}
      toggleSrc={toggleSync}
      srcDesk={desk}
      srcShip={srcShip}
      title={`${appName} Settings`}
    />
  );
};
