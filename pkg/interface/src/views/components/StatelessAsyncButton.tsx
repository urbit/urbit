import React, { ReactElement, ReactNode } from 'react';

import { Button, LoadingSpinner } from '@tlon/indigo-react';

import { useStatelessAsyncClickable } from '~/logic/lib/useStatelessAsyncClickable';

interface AsyncButtonProps {
  children: ReactNode;
  name?: string;
  onClick: (e: React.MouseEvent) => Promise<void>;
}

export function StatelessAsyncButton({
  children,
  onClick,
  name = '',
  disabled = false,
  ...rest
}: AsyncButtonProps & Parameters<typeof Button>[0]): ReactElement {
  const {
    onClick: handleClick,
    buttonState: state
  } = useStatelessAsyncClickable(onClick, name);

  return (
    <Button
      hideDisabled={!disabled}
      disabled={disabled || state === 'loading'}
      onClick={handleClick}
      {...rest}
    >
      {state === 'error' ? (
        'Error'
      ) : state === 'loading' ? (
        <LoadingSpinner
          foreground={
            rest.primary ? 'white' : rest.destructive ? 'red' : 'black'
          }
          background="gray"
        />
      ) : state === 'success' ? (
        'Done'
      ) : (
        children
      )}
    </Button>
  );
}
