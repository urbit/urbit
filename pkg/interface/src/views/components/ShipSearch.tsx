import React, { useMemo, useCallback, ChangeEvent, useState, SyntheticEvent, useEffect } from "react";
import { Box, Label, Icon, Text, Row, Col, ErrorLabel } from "@tlon/indigo-react";
import _ from "lodash";
import ob from "urbit-ob";
import { useField } from "formik";
import styled from "styled-components";

import { DropdownSearch } from "./DropdownSearch";
import { Associations, Association } from "~/types/metadata-update";
import { cite, deSig } from "~/logic/lib/util";
import { Rolodex, Groups } from "~/types";
import { HoverBox } from "./HoverBox";

const INVALID_SHIP_ERR = "Invalid ship";

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
  const [{}, meta, { setValue, setTouched, setError: _setError }] = useField<string[]>({
    name: id,
    multiple: true
  });

  const setError = _setError as unknown as (s: string | undefined) => void;

  const { error, touched } = meta;

  const [selected, setSelected] = useState([] as string[]);
  const [inputShip, setInputShip] = useState(undefined as string | undefined);
  const [inputTouched, setInputTouched] = useState(false);

  const checkInput = useCallback((valid: boolean, ship: string | undefined) => {
    if(valid) {
      setInputShip(ship);
      setError(error === INVALID_SHIP_ERR ? undefined : error);
    } else {
      setError(INVALID_SHIP_ERR);
      setInputTouched(false);
    }
  }, [setError, error, setInputTouched, setInputShip]);

  const onChange = useCallback(
    (e: any) => {
      let ship = `~${deSig(e.target.value) || ""}`;
      if(ob.isValidPatp(ship)) {
        checkInput(true, ship);
      } else {
        checkInput(ship.length !== 1, undefined)
      }
    },
    [checkInput]
  );

  const onBlur = useCallback(() => {
    setInputTouched(true);
  }, [setInputTouched]);

  const onSelect = useCallback(
    (s: string) => {
      setTouched(true);
      checkInput(true, undefined);
      s = `${deSig(s)}`;
      setSelected(v => _.uniq([...v, s]))
    },
    [setTouched, checkInput, setSelected]
  );

  const onRemove = useCallback(
    (s: string) => {
      setSelected(ships => ships.filter(ship => ship !== s))
    },
    [setSelected]
  );

  useEffect(() => {
    const newValue = inputShip ? [...selected, inputShip] : selected;
    setValue(newValue);
  }, [inputShip, selected])

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
          return result ? deSig(s) ?? undefined : undefined;
        }}
        placeholder="Search for ships"
        candidates={peers}
        renderCandidate={renderCandidate}
        disabled={props.maxLength ? selected.length >= props.maxLength : false}
        search={(s: string, t: string) =>
          t.toLowerCase().startsWith(s.toLowerCase())
        }
        getKey={(s: string) => s}
        onSelect={onSelect}
        onChange={onChange}
        onBlur={onBlur}
      />
      <Row minHeight="34px" flexWrap="wrap">
        {selected.map((s) => (
          <Row
            fontFamily="mono"
            alignItems="center"
            py={1}
            px={2}
            color="black"
            borderRadius='2'
            bg='washedGray'
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
      <ErrorLabel
        mt="3"
        hasError={error === INVALID_SHIP_ERR ? inputTouched : !!(touched && error)}>
        {error}
      </ErrorLabel>
    </Col>
  );
}
