import React, {
  useRef,
  useState,
  useMemo,
  useCallback,
  useEffect,
  ChangeEvent,
} from "react";
import _ from "lodash";
import Mousetrap from "mousetrap";
import {
  Box,
  Label,
  ErrorLabel,
  StatelessTextInput as Input
} from "@tlon/indigo-react";
import { useDropdown } from "~/logic/lib/useDropdown";

interface RenderChoiceProps<C> {
  candidate: C;
  onRemove: () => void;
}

interface DropdownSearchProps<C> {
  label: string;
  id: string;
  // check if entry is exact match
  isExact: (s: string) => C | undefined;
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
  // render selected candidate
  renderChoice: (props: RenderChoiceProps<C>) => React.ReactNode;
  onSelect: (c: C) => void;
  onRemove: (c: C) => void;
  value: C | undefined;
  caption?: string;
  disabled?: boolean;
  error?: string;
  placeholder?: string;
}

export function DropdownSearch<C>(props: DropdownSearchProps<C>) {
  const textarea = useRef<HTMLTextAreaElement>();
  const { candidates, getKey, caption } = props;

  const [query, setQuery] = useState("");

  const { next, back, search, selected, options } = useDropdown(
    candidates,
    getKey,
    props.search
  );

  const onSelect = useCallback(
    (c: C) => {
      setQuery("");
      props.onSelect(c);
    },
    [setQuery, props.onSelect]
  );

  const onEnter = useCallback(() => {
    if (selected) {
      onSelect(selected);
    }
    return false;
  }, [onSelect, selected]);

  useEffect(() => {
    if (!textarea.current) {
      return;
    }

    const mousetrap = Mousetrap(textarea.current);
    mousetrap.bind(["down", "tab"], next);
    mousetrap.bind(["up", "shift+tab"], back);
    mousetrap.bind("enter", onEnter);

    return () => {
      mousetrap.unbind(["down", "tab"]);
      mousetrap.unbind(["up", "shift+tab"]);
      mousetrap.unbind("enter", onEnter);
    };
  }, [textarea.current, next, back, onEnter]);

  const onChange = useCallback(
    (e: ChangeEvent<HTMLTextAreaElement>) => {
      search(e.target.value);
      setQuery(e.target.value);
    },
    [setQuery]
  );

  const dropdown = useMemo(() => {
    const first = props.isExact(query);
    let opts = options;
    if (first) {
      opts = options.includes(first) ? opts : [first, ...options];
    }
    return _.take(opts, 5).map((o, idx) =>
      props.renderCandidate(
        o,
        !_.isUndefined(selected) && props.getKey(o) === props.getKey(selected),
        onSelect
      )
    );
  }, [options, props.getKey, props.renderCandidate, selected]);

  return (
    <Box position="relative" zIndex={9}>
      <Label htmlFor={props.id}>{props.label}</Label>
      {caption ? <Label mt="2" gray>{caption}</Label> : null}
      {!props.disabled && (
        <Input
          ref={textarea}
          onChange={onChange}
          value={query}
          autocomplete="off"
          placeholder={props.placeholder || ""}
        />
      )}
      {dropdown.length !== 0 && query.length !== 0 && (
        <Box
          mt={1}
          border={1}
          borderColor="washedGray"
          bg="white"
          width="100%"
          position="absolute"
        >
          {dropdown}
        </Box>
      )}
      {props.value && (
        <Box mt={2} display="flex">
          {props.renderChoice({
            candidate: props.value,
            onRemove: () => props.onRemove(props.value as C),
          })}
        </Box>
      )}
      <ErrorLabel>{props.error}</ErrorLabel>
    </Box>
  );
}

export default DropdownSearch;
