import React from 'react';

import BasicTile from './tiles/basic';
import CustomTile from './tiles/custom';
import ClockTile from './tiles/clock';
import WeatherTile from './tiles/weather';

export default class Tiles extends React.PureComponent {
  render() {
    const { props } = this;

    const tiles = props.tileOrdering.filter((key) => {
      const tile = props.tiles[key];

      return tile.isShown;
    }).map((key) => {
      const tile = props.tiles[key];
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
              weather={props.weather} 
              location={props.location}
            />
          );
        } else if (key === 'clock') {
          return (
            <ClockTile key={key} location={props.location} />
          );
        }
      } else {
        return <CustomTile key={key} />;
      }
    });

    return (
      <React.Fragment>{tiles}</React.Fragment>
    );
  }
}

