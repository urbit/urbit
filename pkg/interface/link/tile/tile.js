import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class LinkTile extends Component {

  render() {
    const { props } = this;

    return (
      <div className="w-100 h-100 relative ba b--black" style={{ background: "#FFFFFF" }}>
        <a className="w-100 h-100 db pa2 no-underline" href="/~link">
          <p
            className="label-regular b absolute"
            style={{ left: 8, top: 4 }}>
            Links
          </p>
          <img
            className="absolute"
            style={{ left: 69, top: 69 }}
            src="/~link/img/Tile.png"
            width={96}
            height={96}
          />
        </a>
      </div>
    );
  }

}

window['link-server-hookTile'] = LinkTile;
