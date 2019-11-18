import React, { Component } from 'react';
import classnames from 'classnames';


export class Skeleton extends Component {
  render() {
    return (
      <div className="h-100 w-100 absolute">
        <div className="cf w-100 absolute flex"
          style={{
            height: 'calc(100% - 48px)'
          }}>
          <div className="h-100 w-100" style={{
            flexGrow: 1,
          }}>
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
}
