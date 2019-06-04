import React, { Component } from 'react';
import classnames from 'classnames';

import { IconHome } from '/components/lib/icons/icon-home';


export class HeaderBar extends Component {

  render() {
    return (
      <div className="bg-black w-100" style={{ height: 48, padding: 8 }}>
        <a className="db"
          style={{ background: '#1A1A1A', borderRadius: 16, width: 32, height: 32 }}
          href='/'>
          <IconHome />
        </a>
      </div>
    );
  }
}


