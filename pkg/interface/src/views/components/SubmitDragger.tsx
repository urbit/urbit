import { Box, Text } from '@tlon/indigo-react';
import React, { ReactElement } from 'react';

const SubmitDragger = (): ReactElement => (
  <Box
    top='0'
    bottom='0'
    left='0'
    right='0'
    position='absolute'
    backgroundColor='white'
    height='100%'
    width='100%'
    display='flex'
    alignItems='center'
    justifyContent='center'
    style={{ pointerEvents: 'none', zIndex: 999 }}
  >
      <Text fontSize='1' color='black'>
        Drop a file to upload
      </Text>
    </Box>
);

export default SubmitDragger;
