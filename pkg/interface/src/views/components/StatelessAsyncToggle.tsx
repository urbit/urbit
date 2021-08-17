import React, { ReactElement } from 'react';

import {
  StatelessToggleSwitchField as Toggle,
  LoadingSpinner,
  Text
} from '@tlon/indigo-react';

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
    <Text mr="2">Error</Text>
  ) : state === 'loading' ? (
    <LoadingSpinner mr="2" foreground={'white'} background="gray" />
  ) : state === 'success' ? (
    <Text mr="2">Done</Text>
  ) : (
    <Toggle onClick={handleClick} {...rest} />
  );
}
