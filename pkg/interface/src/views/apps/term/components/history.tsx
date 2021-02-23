import React, { Component } from 'react';
import { Box } from '@tlon/indigo-react';

import Line from './line';

const History = (props) => {
  return (
    <Box
      height='100%'
      minHeight='0'
      display='flex'
      flexDirection='column-reverse'
      overflowY='scroll'
      style={{ resize: 'none' }}
    >
      <Box
        mt='auto'
      >
        {props.log.map((line, i) => {
          return <Line key={i} line={line} />;
        })}
      </Box>
    </Box>
  );
}

export default History;
