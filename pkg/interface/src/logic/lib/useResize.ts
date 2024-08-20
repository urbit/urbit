import { useEffect, useMemo, useRef } from 'react';
import _ from 'lodash';

export function useResize<T extends HTMLElement>(
  callback: (entry: ResizeObserverEntry, observer: ResizeObserver) => void
) {
  const ref = useRef<T>();

  useEffect(() => {
    function observer(
      entries: ResizeObserverEntry[],
      observer: ResizeObserver
    ) {
      for (const entry of _.flatten(entries)) {
        callback(entry, observer);
      }
    }
    let el = ref.current;
    const resizeObs = new ResizeObserver(observer);
    resizeObs.observe(el, { box: 'border-box' });

    return () => {
      resizeObs.unobserve(el);
    };
  }, [callback]);

  const bind = useMemo(() => ({ ref }), [ref]);

  return bind;
}
