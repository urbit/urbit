import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class ContactTile extends Component {

  render() {
    const { props } = this;

    return (
      <div className="w-100 h-100 relative bg-white b--black ba">
        <a className="w-100 h-100 db pa2 bn" href="/~contacts">
          <p
            className="black absolute f9"
            style={{ left: 8, top: 8 }}>
            Contacts
          </p>
          <img
            className="absolute"
            style={{ left: 39, top: 39 }}
            src="/~contacts/img/Tile.png"
            width={48}
            height={48}
          />
        </a>
      </div>
    );
  }

}

window['contact-viewTile'] = ContactTile;
