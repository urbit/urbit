import { Box } from '@tlon/indigo-react';
import React, { Component } from 'react';
import Line from './line';

export class History extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <Box
        height='100%'
        minHeight={0}
        minWidth={0}
        display='flex'
        flexDirection='column-reverse'
        overflowY='scroll'
        style={{ resize: 'none' }}
      >
        <Box
          mt='auto'
        >
          {/* @ts-ignore declare props in later pass */}
          {this.props.log.map((line, i) => {
            // @ts-ignore react memo not passing props
            return <Line key={i} line={line} />;
          })}
        </Box>
      </Box>
    );
    }
  }

export default History;
