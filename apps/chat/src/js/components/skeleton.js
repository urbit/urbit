import React, { Component } from 'react';
import classnames from 'classnames';

import { HeaderBar } from '/components/lib/header-bar.js';

export class Skeleton extends Component {
  render() {
    return (
      <div className="h-100 w-100 absolute">
        <HeaderBar />
        <div className="cf w-100 absolute flex"
          style={{
            height: 'calc(100% - 48px)' 
          }}>
          <div className="fl h-100 br overflow-x-hidden" style={{ flexBasis: 320 }}>
            {this.props.sidebar}
          </div>
          <div className="h-100 fr" style={{ flexGrow: 1 }}>
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
}

