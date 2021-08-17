import React from 'react';
import { LoadingSpinner, Text } from '@tlon/indigo-react';

const Spinner = ({
  classes = '',
  text = '',
  awaiting = false
}) => awaiting ? (
  <Text zIndex='2' display='flex' className={classes}>
    <LoadingSpinner
      foreground='black'
      background='gray'
      style={{ flexShrink: 0 }}
    />
    <Text display='inline-block' ml='2' verticalAlign='middle' flexShrink='0'>{text}</Text>
  </Text>
) : null;

export { Spinner as default, Spinner };
