import React, { ReactElement } from 'react';
import useLaunchState from '~/logic/state/launch';
import { WeatherState } from '~/types';
import BasicTile from './tiles/basic';
import ClockTile from './tiles/clock';
import CustomTile from './tiles/custom';
import WeatherTile from './tiles/weather';

const Tiles = (): ReactElement => {
  const weather = useLaunchState(state => state.weather) as WeatherState;
  const tileOrdering = useLaunchState(state => state.tileOrdering);
  const tileState = useLaunchState(state => state.tiles);
  const tiles = tileOrdering.filter((key) => {
    const tile = tileState[key];

    return tile.isShown;
  }).map((key) => {
    const tile = tileState[key];
    if ('basic' in tile.type) {
      const basic = tile.type.basic;
      return (
        <BasicTile
          key={key}
          title={basic.title}
          linkedUrl={basic.linkedUrl}
        />
      );
    } else if ('custom' in tile.type) {
      if (key === 'weather') {
        return (
          <WeatherTile key={key} />
        );
      } else if (key === 'clock') {
        const location = weather && 'nearest-area' in weather ? weather['nearest-area'][0] : '';
        return (
          <ClockTile key={key} location={location} />
        );
      } else {
        return (
          <CustomTile
            key={key}
            tileImage={tile.type.custom.image}
            linkedUrl={tile.type.custom.linkedUrl}
          />
        );
      }
    }
    return null;
  });

  return (
    <>{tiles}</>
  );
};

export default Tiles;
