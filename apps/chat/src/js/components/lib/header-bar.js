import React, { Component } from 'react';
import classnames from 'classnames';
import { IconHome } from '/components/lib/icons/icon-home';
import { IconSpinner } from '/components/lib/icons/icon-spinner';

export class HeaderBar extends Component {
  render() {
    let spin = (this.props.spinner)
      ?  <div className="absolute"
           style={{width: 16, height: 16, top: 16, right: 16}}>
           <IconSpinner/>
         </div>
      :  null;

    return (
      <div className="bg-black w-100 justify-between"
        style={{ height: 48, padding: 8}}>
        <a className="db"
          style={{ background: '#1A1A1A',
            borderRadius: 16,
            width: 32,
            height: 32,
            top: 8 }}
          href='/'>
          <IconHome />
        </a>
        {spin}
      </div>
    );
  }
}

