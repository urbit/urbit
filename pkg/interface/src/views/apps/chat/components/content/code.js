import React, { Component } from 'react';
import { Box, Text } from '@tlon/indigo-react';

export default class CodeContent extends Component {
  render() {
    const { props } = this;
    const content = props.content;

    const outputElement =
      (Boolean(content.code.output) &&
       content.code.output.length && content.code.output.length > 0) ?
      (
        <Text
          display='block'
          mono
          p='1'
          my='0'
          borderRadius='1'
          overflow='auto'
          maxHeight='10em'
          maxWidth='100%'
          style={{ whiteSpace: 'pre' }}
          backgroundColor='washedGray'
        >
          {content.code.output[0].join('\n')}
        </Text>
      ) : null;

    return (
      <Box my='2'>
        <Text
          display='block'
          mono
          my='0'
          p='1'
          borderRadius='1'
          overflow='auto'
          maxHeight='10em'
          maxWidth='100%'
          style={{ whiteSpace: 'pre' }}
        >
          {content.code.expression}
        </Text>
        {outputElement}
      </Box>
    );
  }
}
