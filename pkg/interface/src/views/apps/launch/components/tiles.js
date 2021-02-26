import React from 'react';

import BasicTile from './tiles/basic';
import CustomTile from './tiles/custom';
import ClockTile from './tiles/clock';
import WeatherTile from './tiles/weather';

import useLaunchState from '~/logic/state/launch';

const Tiles = (props) => {
  const weather = useLaunchState(state => state.weather);
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
          iconUrl={basic.iconUrl}
          linkedUrl={basic.linkedUrl}
        />
      );
    } else if ('custom' in tile.type) {
      if (key === 'weather') {
        return (
          <WeatherTile
            key={key}
            api={props.api}
          />
        );
      } else if (key === 'clock') {
        const location = weather && 'nearest-area' in weather ? weather['nearest-area'][0] : '';
        return (
          <ClockTile key={key} location={location} />
        );
      }
    } else {
      return <CustomTile key={key} />;
    }
  });

  return (
    <>{tiles}</>
  );
}

export default Tiles;
