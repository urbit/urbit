import React, { memo } from 'react';
import { sigil, reactRenderer } from '@tlon/sigil-js';

export const foregroundFromBackground = (background) => {
  const rgb = {
    r: parseInt(background.slice(1, 3), 16),
    g: parseInt(background.slice(3, 5), 16),
    b: parseInt(background.slice(5, 7), 16)
  };
  const brightness = ((299 * rgb.r) + (587 * rgb.g) + (114 * rgb.b)) / 1000;
  const whiteBrightness = 255;

  return ((whiteBrightness - brightness) < 50) ? 'black' : 'white';
}

export const Sigil = memo(({ classes = '', color, ship, size, svgClass = '', icon = false }) => {
  return ship.length > 14
    ? (<div
      className={'bg-black dib ' + classes}
      style={{ width: size, height: size }}>
    </div>)
    : (<div
      className={'dib ' + classes}
      style={{ flexBasis: size, backgroundColor: color }}
    >
      {sigil({
        patp: ship,
        renderer: reactRenderer,
        size: size,
        icon,
        colors: [
          color,
          foregroundFromBackground(color)
        ],
        class: svgClass
      })}
    </div>)
})

export default Sigil;
