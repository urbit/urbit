import { useEffect } from 'react';
import { useLocation } from 'react-router-dom';

export function useHashLink() {
  const location = useLocation();

  useEffect(() => {
    if(!location.hash) {
      return;
    }
    document.querySelector(location.hash)?.scrollIntoView({ behavior: 'smooth', block: 'start' });
  }, [location.hash]);
}
