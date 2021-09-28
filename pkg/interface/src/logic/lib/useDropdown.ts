import { useCallback, useState } from 'react';

export function useDropdown<C>(
  candidates: C[],
  key: (c: C) => string,
  searchPred: (query: string, c: C) => boolean,
  isExact: (query: string) => C | undefined
) {
  const [options, setOptions] = useState(candidates);
  const [selected, setSelected] = useState<C | undefined>();
  const search = useCallback(
    (s: string) => {
      const exactMatch = isExact(s);
      const exact = exactMatch ? [exactMatch] : [];
      const opts = [...new Set([...exact, ...candidates.filter(c => searchPred(s, c))])];
      setOptions(opts);
      if (selected) {
        const idx = opts.findIndex(c => key(c) === key(selected));
        if (idx < 0) {
          setSelected(undefined);
        }
      }
    },
    [candidates, searchPred, key, selected, setOptions, setSelected]
  );

  const changeSelection = useCallback(
    (backward = false) => {
      const select = (idx: number) => {
        setSelected(options[idx]);
      };
      if(!selected) {
 select(0); return false;
}

      const idx = options.findIndex(c => key(c) === key(selected));
      if (
        idx === -1 ||
        (options.length - 1 <= idx && !backward)
      ) {
        select(0);
      } else if (idx === 0 && backward) {
        select(options.length - 1);
      } else {
        select(idx + (backward ? -1 : 1));
      }
      return false;
    },
    [options, setSelected, selected]
  );

  const next = useCallback(() => changeSelection(), [changeSelection]);
  const back = useCallback(() => changeSelection(true), [changeSelection]);

  return {
    next,
    back,
    search,
    selected,
    options
  };
}
