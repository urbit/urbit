import { useEffect, RefObject } from "react";

export function distanceToBottom(el: HTMLElement) {
  const { scrollTop, scrollHeight, clientHeight } = el;
  const scrolledPercent =
    (scrollHeight - scrollTop - clientHeight) / scrollHeight;
  return _.isNaN(scrolledPercent) ? 0 : scrolledPercent;
}

export function useLazyScroll(
  ref: RefObject<HTMLElement>,
  margin: number,
  loadMore: () => Promise<any>
) {
  useEffect(() => {
    if (!ref.current) {
      return;
    }
    const scroll = ref.current;
    const loadUntil = (el: HTMLElement) => {
      if (distanceToBottom(el) < margin) {
        loadMore().then(() => {
          loadUntil(el);
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
}
