import React, { Component } from 'react';
import { sealDict } from '/components/lib/seal-dict';


export class Sigil extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    let prefix = this.props.prefix ? JSON.parse(this.props.prefix) : false;

    return (
      <div 
        className="bg-black" 
        style={{ flexBasis: 35, padding: 4}}>
      {
        sealDict.getSeal(this.props.ship.slice(1), this.props.size, prefix)
      }
      </div>
    );
  }
}

