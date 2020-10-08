import React, { useMemo, useCallback } from "react";
import { Box, Text } from "@tlon/indigo-react";
import _ from "lodash";
import { useField } from "formik";
import styled from "styled-components";

import { DropdownSearch } from "./DropdownSearch";
import { Associations, Association } from "~/types/metadata-update";

interface InviteSearchProps {
  disabled?: boolean;
  associations: Associations;
  label: string;
  caption?: string;
  id: string;
}

const CandidateBox = styled(Box)<{ selected: boolean }>`
  background-color: ${(p) =>
    p.selected ? p.theme.colors.washedGray : p.theme.colors.white};
  pointer: cursor;
  &:hover {
    background-color: ${(p) => p.theme.colors.washedGray};
  }
`;

const ClickableText = styled(Text)`
  cursor: pointer;

`;

const Candidate = ({ title, selected, onClick }) => (
  <CandidateBox
    onClick={onClick}
    selected={selected}
    borderColor="washedGray"
    color="black"
    fontSize={0}
    p={1}
    width="100%"
  >
    {title}
  </CandidateBox>
);

function renderCandidate(
  a: Association,
  selected: boolean,
  onSelect: (a: Association) => void
) {
  const { title } = a.metadata;

  const onClick = () => {
    onSelect(a);
  };

  return <Candidate title={title} selected={selected} onClick={onClick} />;
}

export function GroupSearch(props: InviteSearchProps) {
  const groups = useMemo(
    () => Object.values(props.associations?.contacts || {}),
    [props.associations?.contacts]
  );

  const [{ value }, { error }, { setValue }] = useField(props.id);

  const group = props.associations?.contacts?.[value];

  const onSelect = useCallback(
    (a: Association) => {
      setValue(a["group-path"]);
    },
    [setValue]
  );

  const onRemove = useCallback(
    (a: Association) => {
      setValue("");
    },
    [setValue]
  );

  return (
    <DropdownSearch<Association>
      label={props.label}
      id={props.id}
      caption={props.caption}
      candidates={groups}
      isExact={() => undefined}
      renderCandidate={renderCandidate}
      disabled={value && value.length !== 0}
      search={(s: string, a: Association) =>
        a.metadata.title.toLowerCase().startsWith(s.toLowerCase())
      }
      getKey={(a: Association) => a["group-path"]}
      onSelect={onSelect}
      onRemove={onRemove}
      renderChoice={({ candidate, onRemove }) => (
        <Box px={2} py={1} border={1} borderColor="washedGrey" color="black" fontSize={0}>
          {candidate.metadata.title}
          <ClickableText ml={2} onClick={onRemove} color="black">
            x
          </ClickableText>
        </Box>
      )}
      value={group}
      error={error}
    />
  );
}

export default GroupSearch;
