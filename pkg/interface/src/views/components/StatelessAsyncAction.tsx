import { Action, LoadingSpinner } from '@tlon/indigo-react';
import React, { ReactNode } from 'react';
import { useStatelessAsyncClickable } from '~/logic/lib/useStatelessAsyncClickable';

interface AsyncActionProps {
  children: ReactNode;
  name?: string;
  disabled?: boolean;
  onClick: (e: React.MouseEvent) => Promise<void>;
}

export function StatelessAsyncAction({
  children,
  onClick,
  name = '',
  disabled = false,
  ...rest
}: AsyncActionProps & Parameters<typeof Action>[0]) {
  const {
    onClick: handleClick,
    buttonState: state
  } = useStatelessAsyncClickable(onClick, name);

  return (
    <Action
      height="18px"
      hideDisabled={!disabled}
      disabled={disabled || state === 'loading'}
      onClick={handleClick} {...rest}
    >
      {state === 'error' ? (
        'Error'
      ) : state === 'loading' ? (
        <LoadingSpinner
          foreground={rest.destructive ? 'red' : 'black'}
          background="transparent"
        />
      ) : state === 'success' ? (
        'Done'
      ) : (
        children
      )}
    </Action>
  );
}
