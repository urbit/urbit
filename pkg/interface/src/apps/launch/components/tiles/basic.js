import React  from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';

import Tile from './tile';

export default class BasicTile extends React.PureComponent {
  render() {
    const { props } = this;

    const children = (
      <span>
        <p className={
           classnames('absolute f9',
           { 'black white-d': props.title !== 'Dojo',
             'white': props.title === 'Dojo' })}
           style={{ left: 8, top: 8 }}
        >
          {props.title}
        </p>
         <img
           className={classnames('absolute',
           { 'invert-d': props.title !== 'Dojo' })}
           style={{ left: 38, top: 38 }}
           src={props.iconUrl}
           width={48}
           height={48}
         />
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
        <div className={classnames('w-100 h-100 relative ba b--black b--gray1-d bg-gray0-d',
        { 'bg-white': props.title !== 'Dojo',
          'bg-black': props.title === 'Dojo' })}
        >{tile}</div>
      </Tile>
    );
  }
}
