import { Box } from '@tlon/indigo-react';
import React, { ReactNode } from 'react';
import { Container } from './Container';

export function Body(
  props: { children: ReactNode } & Parameters<typeof Box>[0]
) {
  const { children, border, ...boxProps } = props;
  return (
    <Box fontSize={0} px={[0, 3]} pb={[0, 3]} height="100%" width="100%">
      <Container
        expand
        round
        border={border ? border : [0, 1]}
        {...boxProps}
      >
        {children}
      </Container>
    </Box>
  );
}
