import { useEffect, ReactNode, useMemo } from 'react';
import { createPortal } from 'react-dom';

export function Portal(props: { children: ReactNode }) {
  const root = document.getElementById('portal-root');

  const el = useMemo(() => document.createElement('div'), []);

  useEffect(() => {
    root?.appendChild(el);
    return () => {
      root?.removeChild(el);
    };
  }, [root, el]);

  return createPortal(props.children, el);
}
