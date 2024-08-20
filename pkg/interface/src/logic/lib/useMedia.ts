import { useCallback, useEffect, useState } from 'react';

export const useMedia = (mediaQuery: string) => {
  const [match, setMatch] = useState(false);

  const update = useCallback((e: MediaQueryListEvent) => {
    setMatch(e.matches);
  }, []);

  useEffect(() => {
    const query = window.matchMedia(mediaQuery);

    query.addEventListener('change', update);
    update({ matches: query.matches } as MediaQueryListEvent);
    return () => {
      query.removeEventListener('change', update);
    };
  }, [update]);

  return match;
};
