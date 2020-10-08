import React, {
  useState,
  useMemo,
  useCallback,
  SyntheticEvent,
  ChangeEvent,
} from "react";
import {
  Col,
  Box,
  Row,
  Text,
  Icon,
  Center,
  Button,
  Action,
  StatelessTextInput as Input,
} from "@tlon/indigo-react";
import _ from "lodash";
import VisibilitySensor from "react-visibility-sensor";

import { Contact, Contacts } from "~/types/contact-update";
import { Sigil } from "~/logic/lib/sigil";
import { cite, uxToHex } from "~/logic/lib/util";
import { Group, RoleTags } from "~/types/group-update";
import { roleForShip, resourceFromPath } from "~/logic/lib/group";
import { Association } from "~/types/metadata-update";
import { useHistory, Link } from "react-router-dom";
import { Dropdown } from "~/views/components/Dropdown";
import GlobalApi from "~/logic/api/global";
import { StatelessAsyncAction } from "~/views/components/StatelessAsyncAction";
import styled from "styled-components";

const TruncText = styled(Box)`
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
`;

type Participant = Contact & { patp: string; pending: boolean };
type ParticipantsTabId = "total" | "pending" | "admin";

const searchParticipant = (search: string) => (p: Participant) => {
  if (search.length == 0) {
    return true;
  }
  const s = search.toLowerCase();
  return p.patp.includes(s) || p.nickname.toLowerCase().includes(search);
};

function getParticipants(cs: Contacts, group: Group) {
  const contacts: Participant[] = _.map(cs, (c, patp) => ({
    ...c,
    patp,
    pending: false,
  }));
  const members: Participant[] = _.map(Array.from(group.members), (m) =>
    emptyContact(m, false)
  );
  const allMembers = _.unionBy(contacts, members, "patp");
  const pending: Participant[] =
    "invite" in group.policy
      ? _.map(Array.from(group.policy.invite.pending), (m) =>
          emptyContact(m, true)
        )
      : [];

  return [
    _.unionBy(allMembers, pending, "patp"),
    pending.length,
    allMembers.length,
  ] as const;
}

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
  api: GlobalApi;
}) {
  const { api } = props;
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

  const ourRole = roleForShip(props.group, window.ship);

  const [filter, setFilter] = useState<ParticipantsTabId>("total");

  const [search, _setSearch] = useState("");
  const setSearch = useMemo(() => _.debounce(_setSearch, 200), [_setSearch]);
  const onSearchChange = useCallback(
    (e: ChangeEvent<HTMLInputElement>) => {
      setSearch(e.target.value);
    },
    [setSearch]
  );

  const adminCount = props.group.tags?.role?.admin?.size || 0;
  const isInvite = "invite" in props.group.policy;

  const [participants, pendingCount, memberCount] = getParticipants(
    props.contacts,
    props.group
  );

  const filtered = useMemo(
    () =>
      _.chain(participants)
        .filter(tabFilters[filter])
        .filter(searchParticipant(search))
        .chunk(8)
        .value(),
    [search, filter, participants]
  );

  // Sticky positioning needs to be disabled on safari due to this bug
  // https://bugs.webkit.org/show_bug.cgi?id=210656
  // TODO: remove when resolved
  const isSafari = useMemo(() => {
    const ua = window.navigator.userAgent;
    return ua.includes("Safari") && !ua.includes("Chrome");
  }, []);

  return (
    <Col height="100%" overflowY="scroll" p={2} position="relative">
      <Row
        bg="white"
        border={1}
        borderColor="washedGray"
        borderRadius={1}
        position={isSafari ? "static" : "sticky"}
        top="0px"
        mb={2}
        px={2}
        zIndex={1}
        flexShrink="0"
      >
        <Row mr="4" flexShrink="0">
          <Tab
            selected={filter}
            setSelected={setFilter}
            id="total"
            label={`${memberCount} total`}
          />
          {isInvite && (
            <Tab
              selected={filter}
              setSelected={setFilter}
              id="pending"
              label={`${pendingCount} pending`}
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
      <Col flexShrink="0" width="100%" height="fit-content">
        <Row alignItems="center" bg="washedGray" borderRadius="1" px="2" my="2">
          <Icon color="gray" icon="MagnifyingGlass" />
          <Input
            maxWidth="256px"
            color="gray"
            bg="transparent"
            border="0"
            placeholder="Search Participants"
            onChange={onSearchChange}
          />
        </Row>
        <Box
          display="grid"
          gridAutoRows={["48px 48px 1px", "48px 1px"]}
          gridTemplateColumns={["48px 1fr", "48px 2fr 1fr", "48px 3fr 1fr"]}
          gridRowGap={2}
          alignItems="center"
        >
          {filtered.map((cs, idx) => (
            <VisibilitySensor
              key={idx}
              offset={{ top: -800, bottom: -800 }}
              partialVisibility
              scrollDelay={150}
            >
              {({ isVisible }) =>
                isVisible ? (
                  cs.map((c) => (
                    <Participant
                      api={api}
                      key={c.patp}
                      role={ourRole}
                      group={props.group}
                      contact={c}
                      association={props.association}
                    />
                  ))
                ) : (
                  <BlankParticipant length={cs.length} />
                )
              }
            </VisibilitySensor>
          ))}
        </Box>
      </Col>
    </Col>
  );
}

function Participant(props: {
  contact: Participant;
  association: Association;
  group: Group;
  role?: RoleTags;
  api: GlobalApi;
}) {
  const { contact, association, group, api } = props;
  const { title } = association.metadata;

  const color = uxToHex(contact.color);
  const isInvite = "invite" in group.policy;

  const role = useMemo(
    () =>
      contact.pending
        ? "pending"
        : roleForShip(group, contact.patp) || "member",
    [contact, group]
  );

  const onPromote = useCallback(async () => {
    const resource = resourceFromPath(association["group-path"]);
    await api.groups.addTag(resource, { tag: "admin" }, [`~${contact.patp}`]);
  }, [api, association]);

  const onDemote = useCallback(async () => {
    const resource = resourceFromPath(association["group-path"]);
    await api.groups.removeTag(resource, { tag: "admin" }, [
      `~${contact.patp}`,
    ]);
  }, [api, association]);

  const onBan = useCallback(async () => {
    const resource = resourceFromPath(association["group-path"]);
    await api.groups.changePolicy(resource, {
      open: { banShips: [`~${contact.patp}`] },
    });
  }, [api, association]);

  return (
    <>
      <Box>
        <Sigil ship={contact.patp} size={32} color={`#${color}`} />
      </Box>
      <Col justifyContent="center" gapY="1" height="100%">
        {contact.nickname && (
          <TruncText title={contact.nickname} maxWidth="85%">
            {contact.nickname}
          </TruncText>
        )}
        <Text title={contact.patp} color="gray" fontFamily="mono">
          {cite(contact.patp)}
        </Text>
      </Col>
      <Row
        justifyContent="space-between"
        gridColumn={["1 / 3", "auto"]}
        alignItems="center"
      >
        <Col>
          <Text color="lightGray" mb="1">
            Role
          </Text>
          <Text>{_.capitalize(role)}</Text>
        </Col>
        <Dropdown
          alignX="right"
          alignY="top"
          options={
            <Col
              bg="white"
              border="1"
              borderRadius="1"
              borderColor="lightGray"
              gapY={2}
              p={2}
            >
              <Action>
                <Link to={`/~chat/new/dm/${contact.patp}`}>
                  <Text color="green">Send Message</Text>
                </Link>
              </Action>
              {props.role === "admin" && (
                <>
                  {!isInvite && (
                    <StatelessAsyncAction onClick={onBan}>
                      <Text color="red">Ban from {title}</Text>
                    </StatelessAsyncAction>
                  )}
                  {role === "admin" ? (
                    <StatelessAsyncAction onClick={onDemote}>
                      Demote from Admin
                    </StatelessAsyncAction>
                  ) : (
                    <StatelessAsyncAction onClick={onPromote}>
                      Promote to Admin
                    </StatelessAsyncAction>
                  )}
                </>
              )}
            </Col>
          }
        >
          <Icon display="block" icon="Ellipsis" />
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

function BlankParticipant({ length }) {
  return (
    <Box
      gridRow={[`auto / span ${3 * length}`, `auto / span ${2 * length}`]}
      gridColumn={["1 / 3", "1 / 4"]}
      height="100%"
    />
  );
}
