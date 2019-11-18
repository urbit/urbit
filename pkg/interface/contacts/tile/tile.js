import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class ContactTile extends Component {

  render() {
    const { props } = this;

    return (
      <div className="w-100 h-100 relative" style={{ background: '#1a1a1a' }}>
        <a className="w-100 h-100 db pa2 no-underline" href="/~chat">
          <p className="gray label-regular b absolute"
             style={{left: 8, top: 4}}>
            Contacts
          </p>
           <img
             className="absolute"
             style={{ left: 68, top: 65 }}
             src="/~chat/img/Tile.png"
             width={106}
             height={98} />
        </a>
      </div>
    );
  }

}

window['contact-viewTile'] = ContactTile;
