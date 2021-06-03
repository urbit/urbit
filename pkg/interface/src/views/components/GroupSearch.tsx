import React, { ReactElement, useMemo, useState } from 'react';
import { useFormikContext, FieldArray } from 'formik';
import _ from 'lodash';
import styled from 'styled-components';

import {
  Box,
  Text,
  Label,
  Row,
  Col,
  Icon,
  ErrorLabel
} from '@tlon/indigo-react';
import { Groups } from '@urbit/api';
import { Associations, Association } from '@urbit/api/metadata';


import { roleForShip } from '~/logic/lib/group';
import { DropdownSearch } from './DropdownSearch';
import useGroupState from '~/logic/state/group';
import useMetadataState from '~/logic/state/metadata';

interface GroupSearchProps<I extends string> {
  disabled?: boolean;
  adminOnly?: boolean;
  publicOnly?: boolean;
  label: string;
  caption?: string;
  id: I;
  maxLength?: number;
}

const CandidateBox = styled(Box)<{ selected: boolean }>`
  &:hover {
    background-color: ${p => p.theme.colors.washedGray};
  }
`;

const Candidate = ({ title, selected, onClick }): ReactElement => (
  <CandidateBox
    onClick={onClick}
    selected={selected}
    cursor="pointer"
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
): ReactElement {
  const { title } = a.metadata;

  const onClick = () => {
    onSelect(a);
  };

  return <Candidate title={title} selected={selected} onClick={onClick} />;
}

type FormValues<I extends string> = {
  [id in I]: string[];
};

export function GroupSearch<I extends string, V extends FormValues<I>>(props: GroupSearchProps<I>): ReactElement {
  const { id, caption, label } = props;
  const {
    values,
    touched: touchedFields,
    errors,
    initialValues,
    setFieldValue,
    setFieldTouched
  } = useFormikContext<V>();
  const [inputIdx, setInputIdx] = useState(initialValues[id].length);
  const name = `${id}[${inputIdx}]`;

  const value: string[] = values[id];
  const touched = touchedFields[id] ?? false;
  const error = _.compact(errors[id] as string[]);
  const groupState = useGroupState(state => state.groups);
  const associations = useMetadataState(state => state.associations);

  const groups: Association[] = useMemo(() => {
     if (props.adminOnly) {
       return Object.values(
          Object.keys(associations.groups)
            .filter(
              e => roleForShip(groupState[e], window.ship) === 'admin'
            )
            .reduce((obj, key) => {
              obj[key] = associations.groups[key];
              return obj;
            }, {}) || {}
        );
     } else if (props.publicOnly) {
       return Object.values(
         Object.keys(associations.groups)
           .filter(
             e => groupState?.[e]?.policy?.open
           )
           .reduce((obj, key) => {
             obj[key] = associations.groups[key];
             return obj;
           }, {}) || {}
       );
     } else {
      return Object.values(associations.groups || {});
     }
  }, [associations.groups]);

  return (
    <FieldArray
      name={id}
      render={(arrayHelpers) => {
        const onSelect = (a: Association) => {
          setFieldValue(name, a.group);
          setFieldTouched(name, true, false);
          setInputIdx(s => s+1);
        };

        const onRemove = (idx: number) => {
          setFieldTouched(name, true, false);
          setInputIdx(s => s - 1);
          arrayHelpers.remove(idx);
        };

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
                disabled={props.maxLength ? value.length >= props.maxLength : false}
                renderCandidate={renderCandidate}
                search={(s: string, a: Association) =>
                  a.metadata.title.toLowerCase().includes(s.toLowerCase())
                }
                getKey={(a: Association) => a.group}
                onSelect={onSelect}
                onBlur={() => {}}
              />
              {value?.length > 0 && (
                value.map((e, idx: number) => {
                  const { title } =
                    associations.groups?.[e]?.metadata || {};
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
                      <Text mr="2">{title || e}</Text>
                      <Icon onClick={() => onRemove(idx)} icon="X" />
                    </Row>
                  );
                })
            )}
            <ErrorLabel hasError={Boolean(touched && error.length > 0)}>
              {error.join(', ')}
            </ErrorLabel>
          </Col>
        );
      }}
    />
  );
}

export default GroupSearch;
