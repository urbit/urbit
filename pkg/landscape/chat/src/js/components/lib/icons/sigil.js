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
        <div style={{ flexBasis: 32, backgroundColor: props.color }}>
          {sigil({
            patp: props.ship,
            renderer: reactRenderer,
            size: props.size,
            colors: [props.color, "white"]
          })}
        </div>
      );
    }
  }
}

