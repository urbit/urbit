import React  from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';

import Tile from './tile';


export default class BasicTile extends React.PureComponent {

  render() {
    const { props } = this;

    const children = (
      <span>
        <p className="black white-d absolute f9"
           style={{left: 8, top: 8}}>{props.title}</p>
         <img
           className="absolute invert-d"
           style={{ left: 38, top: 38 }}
           src={props.iconUrl}
           width={48}
           height={48} />
      </span>
    );

    const routeList = ['/~chat', '/~publish', '/~link', '/~groups', '/~dojo'];

    const tile =  ( routeList.indexOf(props.linkedUrl) !== -1 ) ? (
      <Link className="w-100 h-100 db pa2 no-underline" to={props.linkedUrl}>
       {children}
      </Link>
    ) : (
      <a className="w-100 h-100 db pa2 no-underline" href={props.linkedUrl}>
       {children}
      </a>
    );

    return (
      <Tile>
        <div className={"w-100 h-100 relative bg-white bg-gray0-d ba " +
                        "b--black b--gray1-d"}>{tile}</div>
      </Tile>
    );
  }

}
