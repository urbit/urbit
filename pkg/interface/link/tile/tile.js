import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class LinkTile extends Component {

  render() {
    const { props } = this;

    return (
      <div className="w-100 h-100 relative ba b--black bg-white">
        <a className="w-100 h-100 db pa2 bn" href="/~link">
          <p
            className="f9 black absolute"
            style={{ left: 8, top: 8 }}>
            Links
          </p>
          <img
            className="absolute"
            style={{ left: 39, top: 39 }}
            src="/~link/img/Tile.png"
            width={48}
            height={48}
          />
        </a>
      </div>
    );
  }

}

window['link-server-hookTile'] = LinkTile;
