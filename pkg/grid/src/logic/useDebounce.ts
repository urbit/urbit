import { debounce, DebounceSettings } from 'lodash-es';
import { useRef, useEffect, useCallback } from 'react';
import { useIsMounted } from './useIsMounted';

export function useDebounce(
  cb: (...args: any[]) => void,
  delay: number,
  options?: DebounceSettings
) {
  const isMounted = useIsMounted();
  const inputsRef = useRef({ cb, delay }); // mutable ref like with useThrottle

  useEffect(() => {
    inputsRef.current = { cb, delay };
  }); // also track cur. delay

  return useCallback(
    debounce(
      (...args) => {
        // Debounce is an async callback. Cancel it, if in the meanwhile
        // (1) component has been unmounted (see isMounted in snippet)
        // (2) delay has changed
        if (inputsRef.current.delay === delay && isMounted()) inputsRef.current.cb(...args);
      },
      delay,
      options
    ),
    [delay, debounce]
  );
}
