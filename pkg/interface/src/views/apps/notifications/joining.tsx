import React from 'react';
import { Row, Text, SegmentedProgressBar, Box } from '@tlon/indigo-react';
import {
  JoinProgress,
  joinProgress,
  joinError
} from '@urbit/api';

interface JoiningStatusProps {
  status: JoinProgress;
}

const description: string[] = [
  'Attempting to contact host',
  'Retrieving data',
  'Finished join',
  'Unable to join, you do not have the correct permissions',
  'Internal error, please file an issue'
];

export function JoiningStatus(props: JoiningStatusProps) {
  const { status } = props;

  const current = joinProgress.indexOf(status);
  const desc = description?.[current] || '';
  const isError = joinError.indexOf(status as any) !== -1;
  return (
    <Row
      display={['flex-column', 'flex']}
      alignItems="center"
      px="4"
      gapX="4"
    >
      <Box flexGrow={1} maxWidth="400px">
        <SegmentedProgressBar current={current + 1} segments={3} />
      </Box>
      <Text display="block" flexShrink={0} color={isError ? 'red' : 'gray'}>
        {desc}
      </Text>
    </Row>
  );
}
