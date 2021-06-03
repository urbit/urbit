import React from 'react';
import _ from 'lodash';
import { Box } from '@tlon/indigo-react';
import { PropFunc } from '@urbit/api';

export type Direction = 'East' | 'South' | 'West' | 'North';
type TriangleProps = PropFunc<typeof Box> & {
  direction: Direction;
  color: string;
  size: number;
};

const borders = ['Top', 'Bottom', 'Left', 'Right'] as const;

const directionToBorder = (dir: Direction): typeof borders[number] => {
  switch (dir) {
    case 'East':
      return 'Left';
    case 'West':
      return 'Right';
    case 'North':
      return 'Bottom';
    case 'South':
      return 'Top';
  }
};

const getBorders = (dir: Direction, height: number, color: string) => {
  const solidBorder = directionToBorder(dir);
  const transparent = borders.filter(x => x !== solidBorder);

  return {
    [`border${solidBorder}`]: `${height}px solid`,
    [`border${solidBorder}Color`]: color,
    ..._.mapValues(
      _.keyBy(transparent, border => `border${border}`),
      () => '16px solid transparent'
    )
  };
};

export function Triangle({ direction, color, size, ...rest }: TriangleProps) {
  const borders = getBorders(direction, size, color);

  return <Box width="0px" height="0px" {...borders} {...rest} />;
}
