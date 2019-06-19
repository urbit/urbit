import React, { Component } from 'react';
import classnames from 'classnames';

import { Header } from '/components/header';
import { HeaderBar } from '/components/lib/header-bar';


export class Skeleton extends Component {
  constructor(props){
    super(props);
  }
  //Header {...this.props}/>

  render() {
    return (
      <div className="h-100 w-100 absolute">
        <HeaderBar/>
        <div className="h-inner overflow-y-scroll">
          {this.props.children}
        </div>
      </div>
    );
  }
}
