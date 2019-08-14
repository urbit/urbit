import React, { Component } from 'react';
import { sigil, reactRenderer } from 'urbit-sigil-js';

    
export class Sigil extends Component {
  render() {
    const { props } = this;

    if (props.ship.length > 14) {
      return (
        <div className="bg-black" style={{width: 44, height: 44}}>
        </div>
      );
    } else {
      return (
        <div
          className="bg-black"
          style={{ flexBasis: 35, padding: 4 }}>
        {
          sigil({
            patp: props.ship,
            renderer: reactRenderer,
            size: props.size,
            colors: ['black', 'white'],
          })
        }
        </div>
      );
    }
  }
}

