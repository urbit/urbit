import { BaseImage } from '@tlon/indigo-react';
import React from 'react';
import Tile from './tile';

interface CustomTileProps {
  linkedUrl: string;
  tileImage: string;
}

export default class CustomTile extends React.PureComponent<CustomTileProps> {
  render() {
    const { props } = this;

    return (
      <Tile to={props.linkedUrl} p="0px" boxShadow="none">
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
