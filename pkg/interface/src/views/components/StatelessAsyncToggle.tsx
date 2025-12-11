import {
  Box,
  Icon,
  LoadingSpinner, StatelessToggleSwitchField as Toggle
} from '@tlon/indigo-react';
import React, { ReactElement } from 'react';
import { useStatelessAsyncClickable } from '~/logic/lib/useStatelessAsyncClickable';

interface AsyncToggleProps {
  name?: string;
  onClick: (e: React.MouseEvent) => Promise<void>;
}

export function StatelessAsyncToggle({
  onClick,
  name = '',
  ...rest
}: AsyncToggleProps & Parameters<typeof Toggle>[0]): ReactElement {
  const {
    onClick: handleClick,
    buttonState: state
  } = useStatelessAsyncClickable(onClick, name);

  return state === 'error' ? (
    <Box width={5} textAlign='center' title='Something went wrong...'>
      <Icon icon='ExclaimationMarkBold' />
    </Box>
  ) : state === 'loading' ? (
    <Box width={5} textAlign='center'>
      <LoadingSpinner foreground={'white'} background="gray" />
    </Box>
  ) : state === 'success' ? (
    <Box width={5} textAlign='center' title='Success'>
      <Icon icon='CheckmarkBold' />
    </Box>
  ) : (
    <Toggle onClick={handleClick} {...rest} />
  );
}
