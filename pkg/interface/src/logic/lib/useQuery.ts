import { useMemo, useCallback } from 'react';
import { useLocation } from 'react-router-dom';
import _ from 'lodash';

export function useQuery() {
  const { search } = useLocation();

  const query = useMemo(() => new URLSearchParams(search), [search]);

  const appendQuery = useCallback(
    (q: Record<string, string>) => {
      const newQuery = new URLSearchParams(search);
      _.forIn(q, (value, key) => {
        if (!value) {
          newQuery.delete(key);
        } else {
          newQuery.append(key, value);
        }
      });

      return newQuery.toString();
    },
    [search]
  );

  return {
    query,
    appendQuery
  };
}
