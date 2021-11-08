export type SuspendState = 'result' | 'error' | 'pending';

export interface Suspender<T> {
  read: () => T;
}

export function suspend<T>(awaiting: Promise<T>): Suspender<T> {
  let state: SuspendState = 'pending';
  let result: T | null = null;

  const promise = awaiting
    .then((res) => {
      state = 'result';
      result = res;
    })
    .catch((e) => {
      state = 'error';
      result = e;
    });

  return {
    read: () => {
      if (state === 'result') {
        return result!;
      } else if (state === 'error') {
        throw result;
      } else {
        throw promise;
      }
    }
  };
}

export function suspendWithResult<T>(result: T): Suspender<T> {
  return {
    read: () => result
  };
}
