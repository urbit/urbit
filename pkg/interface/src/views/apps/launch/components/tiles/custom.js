import React  from 'react';
import classnames from 'classnames';

import Tile from './tile';

export default class CustomTile extends React.PureComponent {

  render() {
    const { props } = this;

    return (
      <Tile>
        <div className={"w-100 h-100 relative bg-white bg-gray0-d ba " +
                        "b--black br2 b--gray1-d"}>
          <img
            className="absolute invert-d"
            style={{ left: 38, top: 38 }}
            src={'/~launch/img/UnknownCustomTile.png'}
            width={48}
            height={48} />
        </div>
      </Tile>
    );
  }

}
