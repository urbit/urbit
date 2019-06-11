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
        <HeaderBar/>
        <Header {...this.props}/>
        <div className="h-inner overflow-y-scroll">
          {this.props.children}
        </div>
      </div>
    );
  }
}
