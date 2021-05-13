import _ from 'lodash';
import { useCallback, useMemo } from 'react';
import { useLocation } from 'react-router-dom';

function mergeQuery(search: URLSearchParams, added: Record<string, string>) {
  _.forIn(added, (v, k) => {
    if (v) {
      search.append(k, v);
    } else {
      search.delete(k);
    }
  });
}

export function useQuery() {
  const { search, pathname } = useLocation();

  const query = useMemo(() => new URLSearchParams(search), [search]);

  const appendQuery = useCallback(
    (added: Record<string, string>) => {
      const q = new URLSearchParams(search);
      mergeQuery(q, added);
      return q.toString();
    },
    [search]
  );

  const toQuery = useCallback(
    (params: Record<string, string>, path = pathname) => {
      const q = new URLSearchParams(search);
      mergeQuery(q, params);
      return {
        pathname: path,
        search: q.toString()
      };
    },
    [search, pathname]
  );

  return {
    query,
    appendQuery,
    toQuery
  };
}
