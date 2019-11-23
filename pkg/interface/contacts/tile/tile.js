import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class ContactTile extends Component {

  render() {
    const { props } = this;

    return (
      <div className="w-100 h-100 relative" style={{ background: "#286E55" }}>
        <a className="w-100 h-100 db pa2 no-underline" href="/~contacts">
          <p
            className="white label-regular b absolute"
            style={{ left: 8, top: 4 }}>
            Contacts
          </p>
          <img
            className="absolute"
            style={{ left: 69, top: 69 }}
            src="/~contacts/img/Tile.png"
            width={96}
            height={96}
          />
        </a>
      </div>
    );
  }

}

window['contact-viewTile'] = ContactTile;
