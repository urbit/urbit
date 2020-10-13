import React, { useMemo, useCallback } from "react";
import { Box, Text, Row, Col } from "@tlon/indigo-react";
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
  disabled?: boolean;
  label: string;
  caption?: string;
  id: string;
  contacts: Rolodex;
  groups: Groups;
}

const ClickableText = styled(Text)`
  cursor: pointer;
`;

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
  const [{ value }, { error }, { setValue }] = useField<string[]>(props.id);

  const onSelect = useCallback(
    (s: string) => {
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
      <DropdownSearch<string>
        label={props.label}
        id={props.id}
        isExact={(s) => {
          const ship = `~${deSig(s)}`;
          const result = ob.isValidPatp(ship);
          return result ? deSig(s) : undefined;
        }}
        placeholder="Search for ships"
        caption={props.caption}
        candidates={peers}
        renderCandidate={renderCandidate}
        disabled={false}
        search={(s: string, t: string) =>
          t.toLowerCase().startsWith(s.toLowerCase())
        }
        getKey={(s: string) => s}
        onSelect={onSelect}
        onRemove={onRemove}
        renderChoice={({ candidate, onRemove }) => null}
        value={undefined}
        error={error}
      />
      <Row minHeight="34px" flexWrap="wrap">
        {value.map((s) => (
          <Box
            fontFamily="mono"
            px={2}
            py={1}
            border={1}
            borderColor="washedGrey"
            color="black"
            fontSize={0}
            mt={2}
            mr={2}
          >
            <Text fontFamily="mono">{cite(s)}</Text>
            <ClickableText ml={2} onClick={() => onRemove(s)} color="black">
              x
            </ClickableText>
          </Box>
        ))}
      </Row>
    </Col>
  );
}
