import { useEffect, RefObject, useRef, useState } from "react";
import _ from "lodash";

export function distanceToBottom(el: HTMLElement) {
  const { scrollTop, scrollHeight, clientHeight } = el;
  const scrolledPercent =
    (scrollHeight - scrollTop - clientHeight) / scrollHeight;
  return _.isNaN(scrolledPercent) ? 0 : scrolledPercent;
}

export function useLazyScroll(
  ref: RefObject<HTMLElement>,
  margin: number,
  loadMore: () => Promise<boolean>
) {
  const [isDone, setIsDone] = useState(false);
  useEffect(() => {
    if (!ref.current) {
      return;
    }
    setIsDone(false);
    const scroll = ref.current;
    const loadUntil = (el: HTMLElement) => {
      if (!isDone && distanceToBottom(el) < margin) {
        return loadMore().then((done) => {
          if (done) {
            setIsDone(true);
            return Promise.resolve();
          }
          return loadUntil(el);
        });
      }
      return Promise.resolve();
    };

    loadUntil(scroll);

    const onScroll = (e: Event) => {
      const el = e.currentTarget! as HTMLElement;
      loadUntil(el);
    };

    ref.current.addEventListener("scroll", onScroll);

    return () => {
      ref.current?.removeEventListener("scroll", onScroll);
    };
  }, [ref?.current]);

  return isDone;
}
