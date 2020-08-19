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
  InputLabel,
  ErrorMessage,
  InputCaption,
} from "@tlon/indigo-react";
import { useDropdown } from "~/logic/lib/useDropdown";
import styled from "styled-components";
import { space, color, layout, border } from "styled-system";

interface RenderChoiceProps<C> {
  candidate: C;
  onRemove: () => void;
}

interface DropdownSearchProps<C> {
  label: string;
  id: string;
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
}

const TextArea = styled.input`
  box-sizing: border-box;
  min-width: 0;
  width: 100%;
  resize: none;
  margin-top: ${(p) => p.theme.space[1]}px;
  padding: ${(p) => p.theme.space[2]}px;
  font-size: ${(p) => p.theme.fontSizes[0]}px;
  line-height: 1.2;
  ${space}
  ${color}
  ${layout}
  ${border}
`;

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

  const dropdown = useMemo(
    () =>
      _.take(options, 5).map((o, idx) =>
        props.renderCandidate(
          o,
          !_.isUndefined(selected) &&
            props.getKey(o) === props.getKey(selected),
          onSelect
        )
      ),
    [options, props.getKey, props.renderCandidate, selected]
  );

  return (
    <Box position="relative">
      <InputLabel htmlFor={props.id}>{props.label}</InputLabel>
      {caption ? <InputCaption>{caption}</InputCaption> : null}
      {!props.disabled && (
        <TextArea
          ref={textarea}
          border={1}
          borderColor="washedGray"
          bg="white"
          color="black"
          borderRadius={2}
          onChange={onChange}
          value={query}
          autocomplete="off"
        />
      )}
      {options.length !== 0 && query.length !== 0 && (
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
      <ErrorMessage>{props.error}</ErrorMessage>
    </Box>
  );
}

export default DropdownSearch;
