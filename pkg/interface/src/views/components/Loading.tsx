import React from 'react';
import { Text, Center, LoadingSpinner } from '@tlon/indigo-react';

import { Body } from './Body';

interface LoadingProps {
  text?: string;
}
export function Loading({ text }: LoadingProps) {
  return (
    <Body border="0">
      <Center height="100%">
        <LoadingSpinner />
        {Boolean(text) && <Text ml={4}>{text}</Text>}
      </Center>
    </Body>
  );
}
