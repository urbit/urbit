import { Box } from '@tlon/indigo-react';
import React, { ReactNode } from 'react';

export function Body(
  props: { children: ReactNode } & Parameters<typeof Box>[0]
) {
  const { children, border, ...boxProps } = props;
  return (
    <Box fontSize={0} px={[0, 3]} pb={[0, 3]} height="100%" width="100%" className='body'>
      <Box
        bg="white"
        height="100%"
        width="100%"
        borderRadius={2}
        border={border ? border : [0, 1]}
        borderColor={['lightGray', 'lightGray']}
        {...boxProps}
      >
        {children}
      </Box>
    </Box>
  );
}
