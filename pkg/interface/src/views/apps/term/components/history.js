import React, { Component } from 'react';
import { Box } from '@tlon/indigo-react';

import Line from './line';

export class History extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <Box
        height='100%'
        minHeight='0'
        minWidth='0'
        display='flex'
        flexDirection='column-reverse'
        overflowY='scroll'
        style={{ resize: 'none' }}
      >
        <Box
          mt='auto'
        >
          {this.props.log.map((line, i) => {
            return <Line key={i} line={line} />;
          })}
        </Box>
      </Box>
    );
    }
  }

export default History;
