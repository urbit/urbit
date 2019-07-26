import React, { Component } from 'react';
import { sigil, reactRenderer } from 'urbit-sigil-js';

    
export class Sigil extends Component {
  render() {
    const { props } = this;

    console.log("sigil ship", props.ship);

    if (props.ship.length > 14) {
      return (
        <div className="bg-black" style={{width: 32, height: 32}}>
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

