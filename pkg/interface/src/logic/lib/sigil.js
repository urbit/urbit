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

const sigilBlobUrls = new Map();

export const sigilBlobUrl = (vals) => {
  const { patp, size, icon, color } = vals;
  const key = `${patp}-${size}-${color}${icon ? `-icon` : ''}`;
  if (sigilBlobUrls.has(key)) {
    return sigilBlobUrls.get(key);
  } else {
    const url = SigilURL(vals);
    sigilBlobUrls.set(key, url);
    return url;
  }
};

export const SigilURL = memoize(({ patp, size, icon, colors }) => {
  const sigil = sigiljs({
    patp,
    renderer: stringRenderer,
    size,
    icon,
    colors,
  });
  const blob = new Blob([sigil], { type: 'image/svg+xml'});
  return URL.createObjectURL(blob);
});

export const Sigil = memo(({
  classes = '',
  color,
  foreground = '',
  ship,
  size,
  svgClass = '',
  icon = false,
  padded = false
}) => {
  const padding = (icon && padded) ? '2px' : '0px';
  const innerSize = (icon && padded) ? (Number(size) - 4) : size;
  const foregroundColor = foreground ? foreground : foregroundFromBackground(color);
  return ship.length > 14
    ? (<Box
        backgroundColor='black'
        borderRadius={icon ? '1' : '0'}
        display='inline-block'
        height={size}
        width={size}
        className={classes}
       />) : (
       <Box
        display='flex'
        borderRadius={icon ? '1' : '0'}
        flexBasis={size}
        backgroundColor={color}
        padding={padding}
        className={classes}
        lineHeight={1}
       >
      <BaseImage
        width={size}
        height={size}
        className={svgClass}
        src={sigilBlobUrl({
          patp: ship,
          size: innerSize,
          icon,
          colors: [
            color,
            foregroundColor
          ]
        })}
      />
    </Box>)
})

export default Sigil;
