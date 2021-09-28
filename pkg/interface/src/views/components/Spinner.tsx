import { LoadingSpinner, Text } from '@tlon/indigo-react';
import React from 'react';

const Spinner = ({
  classes = '',
  text = '',
  awaiting = false
}) => awaiting ? (
  <Text zIndex={2} display='flex' className={classes}>
    <LoadingSpinner
      foreground='black'
      background='gray'
    />
    <Text display='inline-block' ml={2} verticalAlign='middle' flexShrink={0}>{text}</Text>
  </Text>
) : null;

export { Spinner as default, Spinner };
