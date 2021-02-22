import { useEffect, RefObject } from 'react';

export function useOutsideClick(
  ref: RefObject<HTMLElement | null | undefined>,
  onClick: () => void
) {
  useEffect(() => {
    function handleClick(event: MouseEvent) {
      const portalRoot = document.querySelector('#portal-root')!;
      if (
        ref.current &&
        !ref.current.contains(event.target as any)
      ) {
        onClick();
      }
    }

    function handleKeyDown(ev) {
      if(ev.key === 'Escape') {
        onClick();
      }
    }
    document.addEventListener('mousedown', handleClick);
    document.addEventListener('keydown', handleKeyDown);

    return () => {
      document.removeEventListener('mousedown', handleClick);
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [ref.current, onClick]);
}
