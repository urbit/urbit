import React, { useMemo, useCallback, useState, useEffect } from 'react';
import {
  Box,
  Text,
  Label,
  Row,
  Col,
  Icon,
  ErrorLabel
} from '@tlon/indigo-react';
import _ from 'lodash';
import { useField } from 'formik';
import styled from 'styled-components';

import { roleForShip } from '~/logic/lib/group';

import { DropdownSearch } from './DropdownSearch';
import { Groups } from '~/types';
import { Associations, Association } from '~/types/metadata-update';

interface InviteSearchProps {
  disabled?: boolean;
  adminOnly: boolean;
  groups: Groups;
  associations: Associations;
  label: string;
  caption?: string;
  id: string;
  maxLength?: number;
}

const CandidateBox = styled(Box)<{ selected: boolean }>`
  &:hover {
    background-color: ${p => p.theme.colors.washedGray};
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
  const [selected, setSelected] = useState([] as string[]);
  const groups: Association[] = useMemo(() => {
    return props.adminOnly
      ? Object.values(
          Object.keys(props.associations?.groups)
            .filter(
              e => roleForShip(props.groups[e], window.ship) === 'admin'
            )
            .reduce((obj, key) => {
              obj[key] = props.associations?.groups[key];
              return obj;
            }, {}) || {}
        )
      : Object.values(props.associations?.groups || {});
  }, [props.associations?.groups]);

  const [{ value }, meta, { setValue, setTouched }] = useField(props.id);

  useEffect(() => {
    setValue(selected);
  }, [selected])

  const { title: groupTitle } =
    props.associations.groups?.[value]?.metadata || {};

  const onSelect = useCallback(
    (s: string) => {
      setTouched(true);
      setSelected(v => _.uniq([...v, s]));
    },
    [setTouched, setSelected]
  );

  const onRemove = useCallback(
    (s: string) => {
      setSelected(groups => groups.filter(group => group !== s))
    },
    [setSelected]
  );

  return (
    <Col>
      <Label htmlFor={id}>{label}</Label>
      {caption && (
        <Label gray mt="2">
          {caption}
        </Label>
      )}
        <DropdownSearch<Association>
          mt="2"
          candidates={groups}
          placeholder="Search for groups..."
          disabled={props.maxLength ? selected.length >= props.maxLength : false}
          renderCandidate={renderCandidate}
          search={(s: string, a: Association) =>
            a.metadata.title.toLowerCase().startsWith(s.toLowerCase())
          }
          getKey={(a: Association) => a.group}
          onSelect={onSelect}
        />
        {value?.length > 0 && (
          value.map((e) => {
            return (
              <Row
                key={e}
                borderRadius="1"
                mt="2"
                width="fit-content"
                border="1"
                borderColor="gray"
                height="32px"
                px="2"
                alignItems="center"
              >
                <Text mr="2">{groupTitle || e}</Text>
                <Icon onClick={onRemove} icon="X" />
              </Row>
            );
          })
      )}
      <ErrorLabel hasError={Boolean(meta.touched && meta.error)}>
        {meta.error}
      </ErrorLabel>
    </Col>
  );
}

export default GroupSearch;
