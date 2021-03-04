import { useEffect, RefObject, useRef, useState } from 'react';
import _ from 'lodash';
import usePreviousValue from './usePreviousValue';

export function distanceToBottom(el: HTMLElement) {
  const { scrollTop, scrollHeight, clientHeight } = el;
  const scrolledPercent =
    (scrollHeight - scrollTop - clientHeight) / scrollHeight;
  return _.isNaN(scrolledPercent) ? 0 : scrolledPercent;
}

export function useLazyScroll(
  ref: RefObject<HTMLElement>,
  margin: number,
  count: number,
  loadMore: () => Promise<boolean>
) {
  const [isDone, setIsDone] = useState(false);
  const [isLoading, setIsLoading] = useState(false);
  const oldCount = usePreviousValue(count);
  const loadUntil = (el: HTMLElement) => {
    if (!isDone && distanceToBottom(el) < margin) {
      setIsLoading(true);
      return loadMore().then((done) => {
        setIsLoading(false);
        if (done) {
          setIsDone(true);
          return Promise.resolve();
        }
        return loadUntil(el);
      });
    }
    setIsLoading(false);
    return Promise.resolve();
  };

  useEffect(() => {
    if((oldCount > count) && ref.current) {
      loadUntil(ref.current);
    }
  }, [count]);

  useEffect(() => {
    if (!ref.current || isDone) {
      return;
    }
    const scroll = ref.current;
    loadUntil(scroll);

    const onScroll = (e: Event) => {
      const el = e.currentTarget! as HTMLElement;
      loadUntil(el);
    };

    ref.current.addEventListener('scroll', onScroll, { passive: true });

    return () => {
      ref.current?.removeEventListener('scroll', onScroll);
    };
  }, [ref?.current, count]);

  return { isDone, isLoading };
}
