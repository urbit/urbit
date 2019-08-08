import React, { Component } from 'react';
import { sigil, reactRenderer } from 'urbit-sigil-js';

    
export class Sigil extends Component {
  render() {
    const { props } = this;

    if (props.ship.length > 14) {
      return (
        <div className="bg-black" style={{width: props.size, height: props.size}}>
        </div>
      );
    } else {
      return (
        <div 
          className="bg-black" 
          style={{ flexBasis: 32 }}>
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

