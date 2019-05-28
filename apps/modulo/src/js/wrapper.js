import React, { Component } from 'react';

export default class Wrapper extends Component {

  constructor() {
    super();
  }

  render() {
    return (
      <div>
        <div style={{ width:'30%', float: 'left' }}>
          <p>Chat</p>
          <p>Chess</p>
        </div>
        <div style={{ width:'70%', float: 'right' }}>
           {this.props.children}
         </div>
       </div>
    );
  }
}

