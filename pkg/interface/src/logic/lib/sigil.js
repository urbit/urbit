import React, { memo } from 'react';
import { sigil as sigiljs, stringRenderer } from '@tlon/sigil-js';
import { memoize } from 'lodash';
import { Box, BaseImage } from '@tlon/indigo-react';

export const foregroundFromBackground = (background) => {
  const rgb = {
    r: parseInt(background.slice(1, 3), 16),
    g: parseInt(background.slice(3, 5), 16),
    b: parseInt(background.slice(5, 7), 16)
  };
  const brightness = ((299 * rgb.r) + (587 * rgb.g) + (114 * rgb.b)) / 1000;
  const whiteBrightness = 255;

  return ((whiteBrightness - brightness) < 50) ? 'black' : 'white';
};

export const SigilURL = memoize(({ patp, size, icon, color}) => {
  const sigil = sigiljs({
    patp,
    renderer: stringRenderer,
    size,
    icon,
    colors: [
      color,
      foregroundFromBackground(color)
    ],
  });
  const blob = new Blob([sigil], { type: 'image/svg+xml'});
  return URL.createObjectURL(blob);
});

export const Sigil = memo(({ classes = '', color, ship, size, svgClass = '', icon = false }) => {
  return ship.length > 14
    ? (<Box
      display='inline-block'
      bg='black'
      className={classes}
      width={size}
      height={size}>
    </Box>)
    : (<Box
      display='inline-block'
      className={classes}
      style={{ flexBasis: size }}
    >
      <BaseImage
        width={size}
        height={size}
        className={svgClass}
        src={SigilURL({ patp: ship, size, icon, color })}
      />
    </Box>)
})

export default Sigil;
