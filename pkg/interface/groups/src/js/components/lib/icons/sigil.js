import React, { Component } from 'react';
import { sigil, reactRenderer } from 'urbit-sigil-js';

export class Sigil extends Component {
  render() {
    const { props } = this;

    const classes = props.classes || '';

    const rgb = {
      r: parseInt(props.color.slice(1, 3), 16),
      g: parseInt(props.color.slice(3, 5), 16),
      b: parseInt(props.color.slice(5, 7), 16)
    };
    const brightness = ((299 * rgb.r) + (587 * rgb.g) + (114 * rgb.b)) / 1000;
    const whiteBrightness = 255;

    let foreground = 'white';

    if ((whiteBrightness - brightness) < 50) {
      foreground = 'black';
    }

    if (props.ship.length > 14) {
      return (
        <div
          className={'bg-black dib ' + classes}
          style={{ width: props.size, height: props.size }}
        ></div>
      );
    } else {
      return (
        <div
          className={'dib ' + classes}
          style={{ flexBasis: props.size, backgroundColor: props.color }}
        >
          {sigil({
            patp: props.ship,
            renderer: reactRenderer,
            size: props.size,
            colors: [props.color, foreground]
          })}
        </div>
      );
    }
  }
}
