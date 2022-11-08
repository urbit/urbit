import _ from 'lodash';
import React from 'react';
import SourceSyncer from '../../components/SourceSyncer';
import { usePike } from '../../state/kiln';

export const SystemUpdatePrefs = () => {
  const desk = 'base';
  const appName = 'your Urbit';
  const pike = usePike(desk);
  const syncShip = pike?.sync?.ship;

  return (
    <SourceSyncer appName={appName} title="System Updates" syncDesk={desk} syncShip={syncShip} />
  );
};
