import { BaseImage, Box } from '@tlon/indigo-react';
import React from 'react';
import Tile from './tile';

export default class CustomTile extends React.PureComponent {
  render() {
    const { props } = this;

    return (
      <Tile to={props.linkedUrl} p="0px">
        <BaseImage
          position='absolute'
          src={props.tileImage}
          width='100%'
          height='100%'
        />
      </Tile>
    );
  }
}
