import {
  LoadingSpinner, StatelessToggleSwitchField as Toggle,

  Text
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
    <Text>Error</Text>
  ) : state === 'loading' ? (
    <LoadingSpinner foreground={'white'} background="gray" />
  ) : state === 'success' ? (
    <Text mx={2}>Done</Text>
  ) : (
    <Toggle onClick={handleClick} {...rest} />
  );
}
