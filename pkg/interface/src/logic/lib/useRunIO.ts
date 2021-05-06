import { useEffect, useState } from 'react';
import { unstable_batchedUpdates } from 'react-dom';

export type IOInstance<I, P, O> = (
  input: I
) => (props: P) => Promise<[(p: P) => boolean, O]>;

export function useRunIO<I, O>(
  io: (i: I) => Promise<O>,
  after: (o: O) => void,
  key: string
): (i: I) => Promise<unknown> {
  const [resolve, setResolve] = useState<() => void>(() => () => {});
  const [reject, setReject] = useState<(e: any) => void>(() => () => {});
  const [output, setOutput] = useState<O | null>(null);
  const [done, setDone] = useState(false);
  const run = (i: I) =>
    new Promise((res, rej) => {
      setResolve(() => res);
      setReject(() => rej);
      io(i)
        .then((o) => {
          unstable_batchedUpdates(() => {
            setOutput(o);
            setDone(true);
          });
        })
        .catch(rej);
    });

  useEffect(() => {
    reject(new Error('useRunIO: key changed'));
    setDone(false);
    setOutput(null);
  }, [key]);

  useEffect(() => {
    if (!done) {
      return;
    }
    try {
      after(output!);
      resolve();
    } catch (e) {
      reject(e);
    }
  }, [done]);

  return run;
}

