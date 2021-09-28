import { useCallback, useEffect, useState } from 'react';

export function useWaitForProps<P>(props: P, timeout = 0) {
  const [resolve, setResolve] = useState<() => void>(() => () => {});
  const [ready, setReady] = useState<(p: P) => boolean | undefined>();

  useEffect(() => {
    if (typeof ready === 'function' && ready(props)) {
      resolve();
    }
  }, [props, ready, resolve]);

  /**
   * Waits until some predicate is true
   *
   * @param r - Predicate to wait for
   * @returns A promise that resolves when `r` returns true, or rejects if the
   * waiting times out
   *
   */
  const waiter = useCallback(
    (r: (props: P) => boolean) => {
      setReady(() => r);
      return new Promise<void>((resolve, reject) => {
        setResolve(() => resolve);
        if(timeout > 0) {
          setTimeout(() => {
            reject(new Error('Timed out'));
          }, timeout);
        }
      });
    },
    [setResolve, setReady, timeout]
  );

  return waiter;
}
