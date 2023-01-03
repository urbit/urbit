import _ from 'lodash';
import React from 'react';
import SourceSetter from '../../components/SourceSetter';
import useKilnState, { usePike } from '../../state/kiln';

export const SystemUpdatePrefs = () => {
  const desk = 'base';
  const appName = 'your Urbit';
  const pike = usePike(desk);
  const srcShip = pike?.sync?.ship;
  const { toggleInstall } = useKilnState();

  return (
    <SourceSetter
      appName={appName}
      toggleSrc={toggleInstall}
      srcDesk={desk}
      srcShip={srcShip}
      title="System Updates"
    />
  );
};
