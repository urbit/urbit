import { useCallback, useState } from 'react';

export type Status = 'initial' | 'loading' | 'success' | 'error';

export function useAsyncCall<ReturnValue>(cb: (...args: any[]) => Promise<ReturnValue>) {
  const [status, setStatus] = useState<Status>('initial');
  const [error, setError] = useState<Error | null>(null);

  const call = useCallback(
    (...args: any[]) => {
      setStatus('loading');
      cb(...args)
        .then((result) => {
          setStatus('success');
          return result;
        })
        .catch((err) => {
          setError(err);
          setStatus('error');
        });
    },
    [cb]
  );

  return {
    call,
    status,
    error
  };
}
