import React from 'react';
import { Box, LoadingSpinner, Text } from '@tlon/indigo-react';

export const BacklogElement = (props) => {
  if (!props.isChatLoading) {
    return null;
  }
  return (
    <Box
      marginLeft='auto'
      marginRight='auto'
      maxWidth='32rem'
      position='absolute'
      zIndex='9999'
      style={{ left: 0, right: 0, top: 0 }}
    >
      <Box
      display='flex'
      justifyContent='center'
      p='3'
      m='3'
      border='1px solid'
      borderColor='washedGray'
      backgroundColor='white'
      >
        <LoadingSpinner
          foreground='black'
          background='gray'
        />
        <Text display='block' ml='3' lineHeight='tall'>Past messages are being restored</Text>
      </Box>
    </Box>
  );
};
