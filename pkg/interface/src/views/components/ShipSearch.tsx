import React, { useMemo, useCallback } from "react";
import { Box, Label, Icon, Text, Row, Col } from "@tlon/indigo-react";
import _ from "lodash";
import ob from "urbit-ob";
import { useField } from "formik";
import styled from "styled-components";

import { DropdownSearch } from "./DropdownSearch";
import { Associations, Association } from "~/types/metadata-update";
import { cite, deSig } from "~/logic/lib/util";
import { Rolodex, Groups } from "~/types";
import { HoverBox } from "./HoverBox";

interface InviteSearchProps {
  autoFocus?: boolean;
  disabled?: boolean;
  label?: string;
  caption?: string;
  id: string;
  contacts: Rolodex;
  groups: Groups;
  hideSelection?: boolean;
  maxLength?: number;
}

const Candidate = ({ title, detail, selected, onClick }) => (
  <HoverBox
    display="flex"
    justifyContent="space-between"
    alignItems="center"
    onClick={onClick}
    selected={selected}
    borderColor="washedGray"
    bgActive="washedGray"
    bg="white"
    color="black"
    fontSize={0}
    p={1}
    width="100%"
  >
    <Text fontFamily="mono">{cite(title)}</Text>
    <Text maxWidth="50%">{detail}</Text>
  </HoverBox>
);

export function ShipSearch(props: InviteSearchProps) {
  const { id, label, caption } = props;
  const [{ value }, { error }, { setValue, setTouched }] = useField<string[]>(
    props.id
  );

  const onSelect = useCallback(
    (s: string) => {
      setTouched(true);
      setValue([...value, s]);
    },
    [setValue, value]
  );

  const onRemove = useCallback(
    (s: string) => {
      setValue(value.filter((v) => v !== s));
    },
    [setValue, value]
  );

  const [peers, nicknames] = useMemo(() => {
    const peerSet = new Set<string>();
    const contacts = new Map<string, string[]>();
    _.forEach(props.groups, (group, path) => {
      if (group.members.size > 0) {
        const groupEntries = group.members.values();
        for (const member of groupEntries) {
          peerSet.add(member);
        }
      }

      const groupContacts = props.contacts[path];

      if (groupContacts) {
        const groupEntries = group.members.values();
        for (const member of groupEntries) {
          if (groupContacts[member]) {
            if (contacts.has(member)) {
              contacts.get(member)?.push(groupContacts[member].nickname);
            } else {
              contacts.set(member, [groupContacts[member].nickname]);
            }
          }
        }
      }
    });
    return [Array.from(peerSet), contacts] as const;
  }, [props.contacts, props.groups]);

  const renderCandidate = useCallback(
    (s: string, selected: boolean, onSelect: (s: string) => void) => {
      const detail = _.uniq(nicknames.get(s)).join(", ");
      const onClick = () => {
        onSelect(s);
      };

      return (
        <Candidate
          title={s}
          detail={detail}
          selected={selected}
          onClick={onClick}
        />
      );
    },
    [nicknames]
  );

  return (
    <Col>
      <Label htmlFor={id}>{label}</Label>
      {caption && (
        <Label gray mt="2">
          {caption}
        </Label>
      )}
      <DropdownSearch<string>
        mt="2"
        isExact={(s) => {
          const ship = `~${deSig(s)}`;
          const result = ob.isValidPatp(ship);
          return result ? deSig(s) : undefined;
        }}
        placeholder="Search for ships"
        candidates={peers}
        renderCandidate={renderCandidate}
        disabled={props.maxLength ? value.length >= props.maxLength : false}
        search={(s: string, t: string) =>
          t.toLowerCase().startsWith(s.toLowerCase())
        }
        getKey={(s: string) => s}
        onSelect={onSelect}
      />
      <Row minHeight="34px" flexWrap="wrap">
        {value.map((s) => (
          <Row
            fontFamily="mono"
            alignItems="center"
            py={1}
            px={2}
            border={1}
            borderColor="washedGrey"
            color="black"
            fontSize={0}
            mt={2}
            mr={2}
          >
            <Text fontFamily="mono">{cite(s)}</Text>
            <Icon
              icon="X"
              ml={2}
              onClick={() => onRemove(s)}
              cursor="pointer"
            />
          </Row>
        ))}
      </Row>
    </Col>
  );
}
