import { Box, BoxProps } from '@tlon/indigo-react';
import React, { ReactElement } from 'react';

type DotProps = BoxProps & {
  color?: string
};

const Dot = ({ color, ...rest }: DotProps): ReactElement => {
  return (
    <Box
      style={{
        backgroundColor: color || 'currentColor',
        width: '4px',
        height: '4px',
        borderRadius: '50%'
      }}
      {...rest}
    />
  );
};

export default Dot;
