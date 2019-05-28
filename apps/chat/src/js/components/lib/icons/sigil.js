import React, { Component } from 'react';
import { sealDict } from '/components/lib/seal-dict';


export class Sigil extends Component {
  render() {
    let prefix = this.props.prefix ? JSON.parse(this.props.prefix) : false;

    return (
      <div 
        className="bg-black" 
        style={{ flexBasis: 35, padding: 4, paddingBottom: 0 }}>
      {
        sealDict.getSeal(this.props.ship, this.props.size, prefix)
      }
      </div>
    );
  }
}

