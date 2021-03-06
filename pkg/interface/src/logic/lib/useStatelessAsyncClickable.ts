import { MouseEvent, useCallback, useEffect, useState } from 'react';
export type AsyncClickableState = 'waiting' | 'error' | 'loading' | 'success';

export function useStatelessAsyncClickable(
  onClick: (e: MouseEvent) => Promise<void>,
  name: string
) {
  const [state, setState] = useState<AsyncClickableState>('waiting');
  const handleClick = useCallback(
    async (e: MouseEvent) => {
      try {
        setState('loading');
        await onClick(e);
        setState('success');
      } catch (e) {
        console.error(e);
        setState('error');
      } finally {
        setTimeout(() => {
          setState('waiting');
        }, 3000);
      }
    },
    [onClick, setState]
  );

  // When name changes, reset button
  useEffect(() => {
    setState('waiting');
  }, [name]);

  return { buttonState: state, onClick: handleClick };
}
