import React, { Component } from 'react';
import classnames from 'classnames';

import { Header } from '/components/header';
import { HeaderBar } from '/components/lib/header-bar';


export class Skeleton extends Component {
  constructor(props){
    super(props);
  }

  render() {
    return (
      <div className="h-100 w-100 absolute">
        <HeaderBar spinner={this.props.spinner}/>
        <div className="h-inner">
          {this.props.children}
        </div>
        <div className="h-footer">
        </div>
      </div>
    );
  }
}
