import { useCallback, useState } from 'react';

export function useToggleState(initial: boolean) {
  const [state, setState] = useState(initial);

  const toggle = useCallback(() => {
    setState(s => !s);
  }, [setState]);

  return [state, toggle] as const;
}
