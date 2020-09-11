import React, { Component } from 'react';
import { sigil, reactRenderer } from 'urbit-sigil-js';

export class Sigil extends Component {
  static foregroundFromBackground(background) {
    const rgb = {
      r: parseInt(background.slice(1, 3), 16),
      g: parseInt(background.slice(3, 5), 16),
      b: parseInt(background.slice(5, 7), 16)
    };
    const brightness = ((299 * rgb.r) + (587 * rgb.g) + (114 * rgb.b)) / 1000;
    const whiteBrightness = 255;

    return ((whiteBrightness - brightness) < 50) ? 'black' : 'white';
  }
  
  render() {
    const { props } = this;

    const classes = props.classes || '';

    const foreground = Sigil.foregroundFromBackground(props.color);

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
            colors: [props.color, foreground],
            class: props.svgClass
          })}
        </div>
      );
    }
  }
}
