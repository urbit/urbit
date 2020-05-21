import React  from 'react';
import classnames from 'classnames';

import Tile from './tile';


export default class BasicTile extends React.PureComponent {

  render() {
    const { props } = this;

    return (
      <Tile>
        <div className={"w-100 h-100 relative bg-white bg-gray0-d ba " +
                        "b--black b--gray1-d"}>
          <a className="w-100 h-100 db pa2 no-underline"
             href={props.linkedUrl}>
            <p className="black white-d absolute f9"
               style={{left: 8, top: 8}}>{props.title}</p>
             <img
               className="absolute invert-d"
               style={{ left: 39, top: 39 }}
               src={props.iconUrl}
               width={48}
               height={48} />
          </a>
        </div>
      </Tile>
    );
  }

}
