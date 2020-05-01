import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class LinkTile extends Component {

  render() {
    return (
      <div className="w-100 h-100 relative ba b--black bg-white">
        <a className="w-100 h-100 db pa2 bn" href="/~eth-wallet">
          <p
            className="f9 black absolute"
            style={{ left: 8, top: 8 }}>
            Wallet
          </p>
          <img
            className="absolute"
            style={{ left: 39, top: 39 }}
            src="/~eth-wallet/img/Tile.png"
            width={48}
            height={48}
          />
        </a>
      </div>
    );
  }

}

window['eth-wallet-viewTile'] = LinkTile;
