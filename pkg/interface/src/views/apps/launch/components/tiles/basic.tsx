import { Icon, Text } from '@tlon/indigo-react';
import React, { ReactElement } from 'react';
import Tile from './tile';

export interface BasicTileProps {
  title: string;
  linkedUrl: string;
}

const BasicTile = (props: BasicTileProps): ReactElement => (
  <Tile
    bg={props.title === 'Terminal' ? '#000000' : 'white'}
    to={props.linkedUrl}
  >
    <Text color={props.title === 'Terminal' ? '#ffffff' : 'black'}>
      {props.title === 'Terminal'
        ? <Icon
          icon='ChevronEast'
          color='#fff'
          size='12px'
          display='inline-block'
          verticalAlign='top'
          mt='5px'
          mr={2}
          />
        : null
      }{props.title}
    </Text>
  </Tile>
);

export default BasicTile;
