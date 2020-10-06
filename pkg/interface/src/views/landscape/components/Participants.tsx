import React, { useState, useMemo, SyntheticEvent, ChangeEvent } from "react";
import {
  Col,
  Box,
  Row,
  Text,
  Icon,
  Center,
  Button,
  Action,
} from "@tlon/indigo-react";
import _ from "lodash";
import { Contact, Contacts } from "~/types/contact-update";
import { Sigil } from "~/logic/lib/sigil";
import { cite, uxToHex } from "~/logic/lib/util";
import { Group, RoleTags } from "~/types/group-update";
import { roleForShip } from "~/logic/lib/group";
import { Association } from "~/types/metadata-update";
import { useHistory, Link } from "react-router-dom";
import { Dropdown } from "~/views/components/Dropdown";

type Participant = Contact & { patp: string; pending: boolean };
type ParticipantsTabId = "total" | "pending" | "admin";

const searchParticipant = (search: string) => (p: Participant) => {
  if (search.length == 0) {
    return true;
  }
  const s = search.toLowerCase();
  return p.patp.includes(s) || p.nickname.toLowerCase().includes(search);
};

const emptyContact = (patp: string, pending: boolean): Participant => ({
  nickname: "",
  email: "",
  phone: "",
  color: "",
  avatar: null,
  notes: "",
  website: "",
  patp,
  pending,
});

const Tab = ({ selected, id, label, setSelected }) => (
  <Box
    py={2}
    borderBottom={selected === id ? 1 : 0}
    borderBottomColor="black"
    mr={2}
    onClick={() => setSelected(id)}
  >
    <Text color={selected === id ? "black" : "gray"}>{label}</Text>
  </Box>
);

export function Participants(props: {
  contacts: Contacts;
  group: Group;
  association: Association;
}) {
  const tabFilters: Record<
    ParticipantsTabId,
    (p: Participant) => boolean
  > = useMemo(
    () => ({
      total: (p) => !p.pending,
      pending: (p) => p.pending,
      admin: (p) => props.group.tags?.role?.admin?.has(p.patp),
    }),
    [props.group]
  );

  const [filter, setFilter] = useState<ParticipantsTabId>("total");

  const [search, _setSearch] = useState("");
  const setSearch = (e: ChangeEvent<HTMLInputElement>) => {
    _setSearch(e.target.value);
  };
  const contacts: Participant[] = useMemo(
    () =>
      _.map(props.contacts, (c, patp) => ({
        ...c,
        patp,
        pending: false,
      })),
    [props.contacts]
  );
  const members: Participant[] = _.map(Array.from(props.group.members), (m) =>
    emptyContact(m, false)
  );
  const allMembers = _.unionBy(contacts, members, "patp");
  const isInvite = "invite" in props.group.policy;
  const pending: Participant[] =
    "invite" in props.group.policy
      ? _.map(Array.from(props.group.policy.invite.pending), (m) =>
          emptyContact(m, true)
        )
      : [];
  const adminCount = props.group.tags?.role?.admin?.size || 0;

  const allSundry = _.unionBy(allMembers, pending, "patp");

  const filtered = _.chain(allSundry)
    .filter(tabFilters[filter])
    .filter(searchParticipant(search))
    .value();

  return (
    <Col height="100%" overflowY="auto" p={2} position="relative">
      <Row
        bg="white"
        border={1}
        borderColor="washedGray"
        borderRadius={1}
        position="sticky"
        top="0px"
        mb={2}
        px={2}
        zIndex={1}
      >
        <Row>
          <Tab
            selected={filter}
            setSelected={setFilter}
            id="total"
            label={`${allMembers.length} total`}
          />
          {isInvite && (
            <Tab
              selected={filter}
              setSelected={setFilter}
              id="pending"
              label={`${pending.length} pending`}
            />
          )}
          <Tab
            selected={filter}
            setSelected={setFilter}
            id="admin"
            label={`${adminCount} Admin${adminCount > 1 ? "s" : ""}`}
          />
        </Row>
      </Row>
      <Box
        display="grid"
        gridAutoRows={["48px 48px 1px", "48px 1px"]}
        gridTemplateColumns={["48px 1fr", "48px 1fr 144px"]}
        gridRowGap={2}
        alignItems="center"
      >
        {filtered.map((c) => (
          <Participant
            key={c.patp}
            role="admin"
            group={props.group}
            contact={c}
            association={props.association}
          />
        ))}
      </Box>
    </Col>
  );
}

function Participant(props: {
  contact: Participant;
  association: Association;
  group: Group;
  role: RoleTags;
}) {
  const history = useHistory();
  const { contact, association, group } = props;
  const { title } = association.metadata;

  const color = uxToHex(contact.color);
  const isInvite = "invite" in group.policy;

  const role = contact.pending
    ? "pending"
    : roleForShip(group, contact.patp) || "member";
  const sendMessage = () => {
    history.push(`/~chat/new/dm/${contact.patp}`);
  };

  return (
    <>
      <Box>
        <Sigil ship={contact.patp} size={32} color={`#${color}`} />
      </Box>
      <Col>
        <Text>{contact.nickname}</Text>
        <Text color="gray" fontFamily="mono">
          {cite(contact.patp)}
        </Text>
      </Col>
      <Row
        width="100%"
        justifyContent="space-between"
        gridColumn={["1 / 3", "auto"]}
        alignItems="center">
        <Col>
          <Text mb={1} color="lightGray">
            Role
          </Text>
          <Text>{_.capitalize(role)}</Text>
        </Col>
        <Dropdown
          alignX="right"
          alignY="top"
          options={
            <Col gapY={1} p={2}>
              <Action onClick={sendMessage}>
                <Text color="green">Send Message</Text>
              </Action>
              {isInvite && (
                <Action onClick={() => {}}>
                  <Text color="red">Ban from {title}</Text>
                </Action>
              )}
              <Action onSelect={() => {}}>Promote to Admin</Action>
            </Col>
          }
        >
          <Icon mr={2} icon="Ellipsis" />
        </Dropdown>
      </Row>
      <Box
        borderBottom={1}
        borderBottomColor="washedGray"
        gridColumn={["1 / 3", "1 / 4"]}
      />
    </>
  );
}

function ParticipantMenu(props: {
  ourRole?: RoleTags;
  theirRole?: RoleTags;
  them: string;
}) {
  const { ourRole, theirRole } = props;
  let options = [];
}
