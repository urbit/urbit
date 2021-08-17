import React, { memo } from 'react';
import { sigil, reactRenderer } from '@tlon/sigil-js';
import { Box } from '@tlon/indigo-react';

export const foregroundFromBackground = (background) => {
  const rgb = {
    r: parseInt(background.slice(1, 3), 16),
    g: parseInt(background.slice(3, 5), 16),
    b: parseInt(background.slice(5, 7), 16)
  };
  const brightness = (299 * rgb.r + 587 * rgb.g + 114 * rgb.b) / 1000;
  const whiteBrightness = 255;

  return whiteBrightness - brightness < 50 ? 'black' : 'white';
};

export const Sigil = memo(
  ({
    classes = '',
    color,
    foreground = '',
    ship,
    size,
    svgClass = '',
    icon = false,
    padding = 0,
    display = 'inline-block'
  }) => {
    const innerSize = Number(size) - 2 * padding;
    const paddingPx = `${padding}px`;
    const foregroundColor = foreground
      ? foreground
      : foregroundFromBackground(color);
    return ship.length > 14 ? (
      <Box
        backgroundColor={color}
        borderRadius={icon ? '1' : '0'}
        display={display}
        height={size}
        width={size}
        className={classes}
      />
    ) : (
      <Box
        display={display}
        borderRadius={icon ? '1' : '0'}
        flexBasis={size}
        backgroundColor={color}
        padding={paddingPx}
        className={classes}
      >
        {sigil({
          patp: ship,
          renderer: reactRenderer,
          size: innerSize,
          icon,
          colors: [color, foregroundColor],
          class: svgClass
        })}
      </Box>
    );
  }
);

Sigil.displayName = 'Sigil';

export default Sigil;
