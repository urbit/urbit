import React  from 'react';
import classnames from 'classnames';
import { Text, Icon } from '@tlon/indigo-react';

import Tile from './tile';

export default class BasicTile extends React.PureComponent {
  render() {
    const { props } = this;

    return (
      <Tile
        bg={props.title === 'Dojo' ? '#000000' : 'white'}
        to={props.linkedUrl}
      >
        <Text color={props.title === 'Dojo' ? '#ffffff' : 'black'}>
          {props.title === 'Dojo'
            ? <Icon
              icon='ChevronEast'
              color='transparent'
              color='#fff'
              style={{ position: 'relative', top: '.3em'}}
            />
            : null
          }{props.title}
        </Text>
      </Tile>
    );
  }
}
