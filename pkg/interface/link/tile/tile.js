import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class LinkTile extends Component {

  render() {
    const unseenCount = this.props.data.unseen || 0;

    let displayUnseen = unseenCount <= 0
      ? null
      : <p
          className="absolute green2 white-d"
          style={{
            bottom: 6,
            fontWeight: 400,
            fontSize: 12,
            lineHeight: "20px"
          }}>
          {unseenCount > 99 ? "99+" : unseenCount}
        </p>;

    return (
      <div className={"w-100 h-100 relative ba b--black b--gray1-d " +
      "bg-white bg-gray0-d"}>
        <a className="w-100 h-100 db pa2 bn" href="/~link">
          <p
            className="f9 black white-d absolute"
            style={{ left: 8, top: 8 }}>
            Links
          </p>
          <img
            className="absolute invert-d"
            style={{ left: 39, top: 39 }}
            src="/~link/img/Tile.png"
            width={48}
            height={48}
          />
          {displayUnseen}
        </a>
      </div>
    );
  }

}

window['link-viewTile'] = LinkTile;
