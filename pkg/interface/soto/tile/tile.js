import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class sotoTile extends Component {

  render() {
    return (
      <div className={"w-100 h-100 relative bg-black bg-gray0-d " +
      "ba b--black b--gray1-d"}>
        <a className="w-100 h-100 db bn" href="/~dojo">
          <p className="white f9 absolute"
           style={{ left: 8, top: 8 }}>
             Dojo
          </p>
          <img src="~dojo/img/Tile.png"
            className="absolute"
            style={{
              left: 39,
              top: 39,
              height: 48,
              width: 48
              }}
          />
        </a>
      </div>
    );
  }

}

window.sotoTile = sotoTile;
