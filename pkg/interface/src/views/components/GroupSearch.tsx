import React, { useMemo, useCallback } from "react";
import {
  Box,
  Text,
  Label,
  Row,
  Col,
  Icon,
  ErrorLabel,
} from "@tlon/indigo-react";
import _ from "lodash";
import { useField } from "formik";
import styled from "styled-components";

import { roleForShip } from "~/logic/lib/group";

import { DropdownSearch } from "./DropdownSearch";
import { Groups } from "~/types";
import { Associations, Association } from "~/types/metadata-update";

interface InviteSearchProps {
  disabled?: boolean;
  adminOnly: boolean;
  groups: Groups;
  associations: Associations;
  label: string;
  caption?: string;
  id: string;
}

const CandidateBox = styled(Box)<{ selected: boolean }>`
  &:hover {
    background-color: ${(p) => p.theme.colors.washedGray};
  }
`;

const Candidate = ({ title, selected, onClick }) => (
  <CandidateBox
    onClick={onClick}
    selected={selected}
    borderColor="washedGray"
    color="black"
    fontSize={0}
    p={2}
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
  const { id, caption, label } = props;
  const groups: Association[] = useMemo(() => {
    return props.adminOnly
      ? Object.values(
          Object.keys(props.associations?.contacts)
            .filter(
              (e) => roleForShip(props.groups[e], window.ship) === "admin"
            )
            .reduce((obj, key) => {
              obj[key] = props.associations?.contacts[key];
              return obj;
            }, {}) || {}
        )
      : Object.values(props.associations?.contacts || {});
  }, [props.associations?.contacts]);

  const [{ value }, meta, { setValue, setTouched }] = useField(props.id);

  const { title: groupTitle } =
    props.associations.contacts?.[value]?.metadata || {};

  const onSelect = useCallback(
    (a: Association) => {
      setValue(a["group-path"]);
      setTouched(true);
    },
    [setValue]
  );

  const onUnselect = useCallback(() => {
    setValue(undefined);
    setTouched(true);
  }, [setValue]);

  return (
    <Col>
      <Label htmlFor={id}>{label}</Label>
      {caption && (
        <Label gray mt="2">
          {caption}
        </Label>
      )}
      {value && (
        <Row
          borderRadius="1"
          mt="2"
          width="fit-content"
          border="1"
          borderColor="gray"
          height="32px"
          px="2"
          alignItems="center"
        >
          <Text mr="2">{groupTitle || value}</Text>
          <Icon onClick={onUnselect} icon="X" />
        </Row>
      )}
      {!value && (
        <DropdownSearch<Association>
          mt="2"
          candidates={groups}
          renderCandidate={renderCandidate}
          search={(s: string, a: Association) =>
            a.metadata.title.toLowerCase().startsWith(s.toLowerCase())
          }
          getKey={(a: Association) => a["group-path"]}
          onSelect={onSelect}
        />
      )}
      <ErrorLabel hasError={!!(meta.touched && meta.error)}>
        {meta.error}
      </ErrorLabel>
    </Col>
  );
}

export default GroupSearch;
