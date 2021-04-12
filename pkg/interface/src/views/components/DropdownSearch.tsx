import React, {
  useRef,
  useState,
  useMemo,
  useCallback,
  useEffect,
  ChangeEvent,
  ReactElement
} from 'react';
import _ from 'lodash';
import Mousetrap from 'mousetrap';

import {
  Box,
  StatelessTextInput as Input
} from '@tlon/indigo-react';

import { useDropdown } from '~/logic/lib/useDropdown';
import { PropFunc } from '~/types/util';

interface DropdownSearchExtraProps<C> {
  // check if entry is exact match
  isExact?: (s: string) => C | undefined;
  // Options for dropdown
  candidates: C[];
  // Present options in dropdown
  renderCandidate: (
    c: C,
    selected: boolean,
    onSelect: (c: C) => void
  ) => React.ReactNode;
  // get a unique key for comparisons/react lists
  getKey: (c: C) => string;
  // search predicate
  search: (s: string, c: C) => boolean;
  onSelect: (c: C) => void;
  disabled?: boolean;
  placeholder?: string;
  onChange?: (e: ChangeEvent<HTMLTextAreaElement>) => void;
  onBlur?: (e: FocusEvent) => void;
  onFocus?:  (e: FocusEvent) => void;
}

type DropdownSearchProps<C> = PropFunc<typeof Box> &
  DropdownSearchExtraProps<C>;

export function DropdownSearch<C>(props: DropdownSearchProps<C>): ReactElement {
  const textarea = useRef<HTMLTextAreaElement>();
  const {
    candidates,
    getKey,
    search: searchPred,
    onSelect,
    isExact,
    renderCandidate,
    disabled,
    placeholder,
    onFocus = (): void => {},
    onChange = (): void => {},
    onBlur = (): void => {},
    ...rest
  } = props;

  const [query, setQuery] = useState('');
  const exact = useCallback(
    (s: string) => {
      return isExact ? isExact(s) : undefined;
    },
    [isExact]
  );

  const { next, back, search, selected, options } = useDropdown(
    candidates,
    getKey,
    searchPred,
    exact
  );

  const handleSelect = useCallback(
    (c: C) => {
      setQuery('');
      onSelect(c);
    },
    [setQuery, onSelect]
  );

  const onEnter = useCallback(() => {
    if (selected) {
      handleSelect(selected);
    }
    return false;
  }, [handleSelect, selected]);

  useEffect(() => {
    if (!textarea.current) {
      return;
    }

    const mousetrap = Mousetrap(textarea.current);
    mousetrap.bind(['down', 'tab'], next);
    mousetrap.bind(['up', 'shift+tab'], back);
    mousetrap.bind('enter', onEnter);

    return () => {
      mousetrap.unbind(['down', 'tab']);
      mousetrap.unbind(['up', 'shift+tab']);
      mousetrap.unbind('enter');
    };
  }, [textarea.current, next, back, onEnter]);

  const changeCallback = useCallback(
    (e: ChangeEvent<HTMLTextAreaElement>) => {
      onChange(e);
      search(e.target.value);
      setQuery(e.target.value);
    },
    [search, onChange]
  );

  const dropdown = useMemo(() => {
    const first = props.isExact?.(query);
    let opts = options;
    if (first) {
      opts = options.includes(first) ? opts : [first, ...options];
    }
    return _.take(opts, 5).map((o, idx) =>
      props.renderCandidate(
        o,
        !_.isUndefined(selected) && props.getKey(o) === props.getKey(selected),
        handleSelect
      )
    );
  }, [options, props.getKey, props.renderCandidate, selected]);

  return (
    <Box {...rest} position="relative" zIndex={9}>
      <Input
        ref={textarea}
        onChange={changeCallback}
        value={query}
        autocomplete="off"
        disabled={disabled}
        placeholder={placeholder}
        onBlur={onBlur}
      />
      {dropdown.length !== 0 && query.length !== 0 && (
        <Box
          mt="1"
          border="1"
          borderRadius="1"
          borderColor="washedGray"
          bg="white"
          width="100%"
          position="absolute"
        >
          {dropdown}
        </Box>
      )}
    </Box>
  );
}

export default DropdownSearch;
