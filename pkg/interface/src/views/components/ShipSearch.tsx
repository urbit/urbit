import React, {
  useMemo,
  useCallback,
  ChangeEvent,
  useState,
  SyntheticEvent,
  useEffect,
  useRef,
} from "react";
import {
  Box,
  Label,
  Icon,
  Text,
  Row,
  Col,
  ErrorLabel,
} from "@tlon/indigo-react";
import _ from "lodash";
import ob from "urbit-ob";
import * as Yup from "yup";
import { useField, FieldArray, useFormikContext } from "formik";
import styled from "styled-components";

import { DropdownSearch } from "./DropdownSearch";
import { Associations, Association } from "~/types/metadata-update";
import { cite, deSig } from "~/logic/lib/util";
import { Rolodex, Groups } from "~/types";
import { HoverBox } from "./HoverBox";

const INVALID_SHIP_ERR = "Invalid ship";

interface InviteSearchProps<I extends string> {
  autoFocus?: boolean;
  disabled?: boolean;
  label?: string;
  caption?: string;
  id: I;
  contacts: Rolodex;
  groups: Groups;
  hideSelection?: boolean;
  maxLength?: number;
}

const getNicknameForShips = (groups: Groups, contacts: Rolodex) => {
  const peerSet = new Set<string>();
  const nicknames = new Map<string, string[]>();
  _.forEach(groups, (group, path) => {
    if (group.members.size > 0) {
      const groupEntries = group.members.values();
      for (const member of groupEntries) {
        peerSet.add(member);
      }
    }

    const groupContacts = contacts[path];

    if (groupContacts) {
      const groupEntries = group.members.values();
      for (const member of groupEntries) {
        if (groupContacts[member]) {
          if (nicknames.has(member)) {
            nicknames.get(member)?.push(groupContacts[member].nickname);
          } else {
            nicknames.set(member, [groupContacts[member].nickname]);
          }
        }
      }
    }
  });
  return [Array.from(peerSet), nicknames] as const;
};

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

type Value<I extends string> = {
  [k in I]: string[];
};

const shipItemSchema = Yup.string().test(
  "is-patp",
  "${value} is not a valid @p",
  x => ob.isValidPatp(`~${x}`)
);

export const shipSearchSchema = Yup.array(shipItemSchema).compact();

export const shipSearchSchemaInGroup = (members: string[]) =>
  Yup.array(shipItemSchema.oneOf(members, "${value} not a member of this group")).compact();

export function ShipSearch<I extends string, V extends Value<I>>(
  props: InviteSearchProps<I>
) {
  const { id, label, caption } = props;
  const {
    values,
    touched,
    errors,
    initialValues,
    setFieldValue,
  } = useFormikContext<V>();

  const inputIdx = useRef(initialValues[id].length);

  const selected: string[] = values[id] ?? [];

  const name = () => `${props.id}[${inputIdx.current}]`;

  const pills = selected.slice(0, inputIdx.current);

  const [peers, nicknames] = useMemo(
    () => getNicknameForShips(props.groups, props.contacts),
    [props.contacts, props.groups]
  );

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

  const onChange = (e: ChangeEvent<HTMLTextAreaElement>) => {
    const newValue =
      e.target.value?.length > 0 ? `~${deSig(e.target.value)}` : "";
    setFieldValue(name(), newValue);
  };

  const error = _.compact(errors[id] as string[]);

  return (
    <FieldArray
      name={id}
      render={(arrayHelpers) => {
        const onAdd = (ship: string) => {
          setFieldValue(name(), ship);
          inputIdx.current += 1;
          arrayHelpers.push("");
        };

        const onRemove = (idx: number) => {
          inputIdx.current -= 1;
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
              disabled={
                props.maxLength ? selected.length >= props.maxLength : false
              }
              search={(s: string, t: string) =>
                (t || "").toLowerCase().startsWith(s.toLowerCase())
              }
              getKey={(s: string) => s}
              onChange={onChange}
              onSelect={onAdd}
            />
            <Row minHeight="34px" flexWrap="wrap">
              {pills.map((s, i) => (
                <Row
                  fontFamily="mono"
                  alignItems="center"
                  py={1}
                  px={2}
                  color="black"
                  borderRadius="2"
                  bg="washedGray"
                  fontSize={0}
                  mt={2}
                  mr={2}
                >
                  <Text fontFamily="mono">{cite(s)}</Text>
                  <Icon
                    icon="X"
                    ml={2}
                    onClick={() => onRemove(i)}
                    cursor="pointer"
                  />
                </Row>
              ))}
            </Row>
            <ErrorLabel mt="3" hasError={error.length > 0}>
              {error.join(", ")}
            </ErrorLabel>
          </Col>
        );
      }}
    />
  );
}
