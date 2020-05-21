import React from 'react';
import classnames from 'classnames';

import BasicTile from './tiles/basic';
import CustomTile from './tiles/custom';
import ClockTile from './tiles/clock';
import WeatherTile from './tiles/weather';


export default class Tiles extends React.PureComponent {
  render() {
    const { props } = this;
    console.log('render');
    
    let tiles = props.tileOrdering.filter((key) => {
      return props.tiles[key].isShown;
    }).map((key) => {
      let tile = props.tiles[key];
      if ('basic' in tile.type) {
        let basic = tile.type.basic;
        return (
          <BasicTile
            key={key}
            title={basic.title}
            iconUrl={basic.iconUrl}
            linkedUrl={basic.linkedUrl} />
        );
      } else if ('custom' in tile.type) {
        if (key === 'weather') {
          return (<WeatherTile key={key} api={props.api} />);
        } else if (key === 'clock') {
          return (<ClockTile key={key} />);
        }
      } else {
        return <CustomTile key={key} />;
      }
    });

    return (
      <div>{tiles}</div>
    );
  }
}



