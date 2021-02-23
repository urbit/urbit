import React from 'react';

import { CodeContent } from '@urbit/api';
import { Box, Text } from '@tlon/indigo-react';

interface CodeContentProps {
  content: CodeContent
}

const CodeContentComponent = (props: CodeContentProps) => {
  const { content } = props;

  const outputElement =
    (!!content.code.output &&
     content.code.output.length && content.code.output.length > 0) ?
    (
      <Text
        display='block'
        fontSize='0'
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
        {content.code.output.join('\n')}
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
        fontSize='0'
        style={{ whiteSpace: 'pre' }}
      >
        {content.code.expression}
      </Text>
      {outputElement}
    </Box>
  );
}

export default CodeContentComponent;