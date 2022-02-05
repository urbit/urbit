import {
    Action, Box, Col,

    Icon,
    Image, Row,

    StatelessTextInput as Input, Text
} from '@tlon/indigo-react';
import { addTag, Association, Contact, Contacts, changePolicy, deSig, Group, removeMembers, removeTag, RoleTags } from '@urbit/api';
import _ from 'lodash';
import f from 'lodash/fp';
import React, {
    ChangeEvent,
    ReactElement, useCallback, useMemo, useState
} from 'react';
import { Link } from 'react-router-dom';
import VisibilitySensor from 'react-visibility-sensor';
import styled from 'styled-components';
import { resourceFromPath, roleForShip } from '~/logic/lib/group';
import { Sigil } from '~/logic/lib/sigil';
import { cite, uxToHex } from '~/logic/lib/util';
import useContactState from '~/logic/state/contact';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import { Dropdown } from '~/views/components/Dropdown';
import { StatelessAsyncAction } from '~/views/components/StatelessAsyncAction';
import airlock from '~/logic/api';

const TruncText = styled(Text)`
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  display: inline-block;
  min-width: 0;
`;

type Participant = Contact & { patp: string; pending: boolean };
type ParticipantsTabId = 'total' | 'pending' | 'admin';

const searchParticipant = (search: string) => (p: Participant) => {
  if (search.length == 0) {
    return true;
  }
  let s = search.toLowerCase();
  s = (s.startsWith('~')) ? s.substr(1) : s;
  return p.patp.includes(s) || p.nickname.toLowerCase().includes(s);
};

const emptyContact = (patp: string, pending: boolean): Participant => ({
  nickname: '',
  bio: '',
  status: '',
  color: '0x0',
  avatar: null,
  cover: null,
  groups: [],
  patp,
  'last-updated': 0,
  pending
});

function getParticipants(cs: Contacts, group: Group) {
  const contacts: Participant[] = _.flow(
    f.omitBy<Contacts>((_c, patp) => !group.members.has(deSig(patp))),
    f.toPairs,
    f.map(([patp, c]: [string, Contact]) => ({
      ...c,
      patp: deSig(patp),
      pending: false
    }))
  )(cs);
  const members: Participant[] = _.map(
    Array.from(group.members),
    s => contacts[s] ?? emptyContact(s, false)
  );
  const allMembers = _.unionBy(contacts, members, 'patp');
  const pending: Participant[] =
    'invite' in group.policy
      ? _.map(Array.from(group.policy.invite.pending), m =>
          emptyContact(m, true)
        )
      : [];

  const incPending = _.unionBy(allMembers, pending, 'patp');
  return [
    incPending,
    incPending.length - group.members.size,
    group.members.size
  ] as const;
}

const Tab = ({ selected, id, label, setSelected }) => (
  <Box
    py={2}
    borderBottom={selected === id ? 1 : 0}
    borderBottomColor="black"
    mr={2}
    cursor="pointer"
    onClick={() => setSelected(id)}
  >
    <Text color={selected === id ? 'black' : 'gray'}>{label}</Text>
  </Box>
);

export function Participants(props: {
  group: Group;
  association: Association;
}): ReactElement {
  const tabFilters: Record<
    ParticipantsTabId,
    (p: Participant) => boolean
  > = useMemo(
    () => ({
      total: p => !p.pending,
      pending: p => p.pending,
      admin: p => props.group.tags?.role?.admin?.has(p.patp)
    }),
    [props.group]
  );

  const ourRole = roleForShip(props.group, window.ship);

  const [filter, setFilter] = useState<ParticipantsTabId>('total');

  const [search, _setSearch] = useState('');
  const setSearch = useMemo(() => _.debounce(_setSearch, 200), [_setSearch]);
  const onSearchChange = useCallback(
    (e: ChangeEvent<HTMLInputElement>) => {
      setSearch(e.target.value);
    },
    [setSearch]
  );

  const adminCount = props.group.tags?.role?.admin?.size || 0;
  const isInvite = 'invite' in props.group.policy;
  const contacts = useContactState(state => state.contacts);

  const [participants, pendingCount, memberCount] = getParticipants(
    contacts,
    props.group
  );

  const filtered = useMemo(
    () =>
      f.flow(
        f.filter(tabFilters[filter]),
        f.filter(searchParticipant(search)),
        f.chunk(8)
      )(participants),
    [search, filter, participants]
  );

  // Sticky positioning needs to be disabled on safari due to this bug
  // https://bugs.webkit.org/show_bug.cgi?id=210656
  // TODO: remove when resolved
  const isSafari = useMemo(() => {
    const ua = window.navigator.userAgent;
    return ua.includes('Safari') && !ua.includes('Chrome');
  }, []);

  return (
    <Col height="100%" overflowY="scroll" p={2} position="relative">
      <Row
        bg="white"
        border={1}
        borderColor="washedGray"
        borderRadius={1}
        position={isSafari ? 'static' : 'sticky'}
        top="0px"
        mb={2}
        px={2}
        zIndex={1}
        flexShrink={0}
      >
        <Row mr={4} flexShrink={0}>
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
            label={`${adminCount} Admin${adminCount > 1 ? 's' : ''}`}
          />
        </Row>
      </Row>
      <Col flexShrink={0} width="100%" height="fit-content">
        <Row alignItems="center" bg="washedGray" borderRadius={1} px={2} my={2}>
          <Icon color="gray" icon="Search" />
          <Input
            maxWidth="256px"
            color="gray"
            bg="transparent"
            border={0}
            placeholder="Search Participants"
            onChange={onSearchChange}
          />
        </Row>
        <Col alignItems="center" >
          {filtered.map((cs, idx) => (
            <VisibilitySensor
              key={idx}
              offset={{ top: -800, bottom: -800 }}
              partialVisibility
              scrollDelay={150}
            >
              {({ isVisible }) =>
                isVisible ? (
                  cs.map(c => (
                    <Participant
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
        </Col>
      </Col>
    </Col>
  );
}

function Participant(props: {
  contact: Participant;
  association: Association;
  group: Group;
  role?: RoleTags;
}) {
  const { contact, association, group } = props;
  const { title } = association.metadata;
  const { hideAvatars, hideNicknames } = useSettingsState(selectCalmState);

  const color = uxToHex(contact.color);
  const isInvite = 'invite' in group.policy;

  const role = useMemo(
    () =>
      contact.pending
        ? 'pending'
        : roleForShip(group, contact.patp) || 'member',
    [contact, group]
  );

  const onPromote = useCallback(async () => {
    const resource = resourceFromPath(association.group);
    await airlock.poke(addTag(resource, { tag: 'admin' }, [`~${contact.patp}`]));
  }, [contact.patp, association]);

  const onDemote = useCallback(async () => {
    const resource = resourceFromPath(association.group);
    await airlock.poke(removeTag({ tag: 'admin' }, resource, [`~${contact.patp}`]));
  }, [association, contact.patp]);

  const onBan = useCallback(async () => {
    const resource = resourceFromPath(association.group);
    await airlock.poke(changePolicy(resource, {
      open: { banShips: [`~${contact.patp}`] }
    }));
  }, [association, contact.patp]);

  const onKick = useCallback(async () => {
    const resource = resourceFromPath(association.group);
    if(contact.pending) {
      await airlock.poke(changePolicy(
        resource,
        { invite: { removeInvites: [`~${contact.patp}`] } }
      ));
    } else {
      await airlock.poke(removeMembers(resource, [`~${contact.patp}`]));
    }
  }, [contact, association]);

  const avatar =
    contact?.avatar && !hideAvatars ? (
      <Image
        src={contact.avatar}
        height={32}
        width={32}
        display='inline-block'
        style={{ objectFit: 'cover' }}
      />
    ) : (
      <Sigil ship={contact.patp} size={32} color={`#${color}`} />
    );

  const hasNickname = contact.nickname && !hideNicknames;

  return (
    <>
      <Row flexDirection={['column', 'row']} gapX={2} alignItems={['flex-start', 'center']} width="100%" justifyContent="space-between" height={['96px', '60px']}>
        <Row gapX={4} alignItems="center" height="100%">
      <Box>{avatar}</Box>
      <Col alignItems="self-start" justifyContent="center" gapY={1} height="100%" minWidth={0}>
        {hasNickname && (
          <Row minWidth={0} flexShrink={1}>
          <TruncText title={contact.nickname} color="black">
            {contact.nickname}
          </TruncText>
          </Row>
        )}
        <Text title={contact.patp} color="gray" fontFamily="mono">
          {cite(contact.patp)}
        </Text>
      </Col>
    </Row>
      <Row
        justifyContent="space-between"
        width={['100%', '128px']}
        alignItems="center"
        gapX={4}
      >
        <Col>
          <Text color="lightGray" mb={1}>
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
              border={1}
              borderRadius={1}
              borderColor="lightGray"
              gapY={2}
              p={2}
            >
              <Action bg="transparent">
                <Link to={`/~profile/~${contact.patp}`}>
                  <Text color="black">View Profile</Text>
                </Link>
              </Action>
              <Action bg="transparent">
                <Link to={`/~landscape/messages/dm/~${contact.patp}`}>
                  <Text color="green">Send Message</Text>
                </Link>
              </Action>
              {props.role === 'admin' && (
                <>
                  {(!isInvite && contact.patp !== window.ship) && (
                    <StatelessAsyncAction onClick={onBan} bg="transparent">
                      <Text color="red">Ban from {title}</Text>
                    </StatelessAsyncAction>
                  )}
                  {role === 'admin' ? (
                    group?.tags?.role?.admin?.size > 1 && (<StatelessAsyncAction onClick={onDemote} bg="transparent">
                      Demote from Admin
                    </StatelessAsyncAction>)
                  ) : (
                    <>
                    {(contact.patp !== window.ship) && (<StatelessAsyncAction onClick={onKick} bg="transparent">
                        <Text color="red">Kick from {title}</Text>
                      </StatelessAsyncAction>)}
                      {!contact.pending && <StatelessAsyncAction onClick={onPromote} bg="transparent">
                        Promote to Admin
                      </StatelessAsyncAction>}
                    </>
                  )}
                </>
              )}
            </Col>
          }
        >
          <Icon display="block" icon="Ellipsis" />
        </Dropdown>
      </Row>
    </Row>
      <Box
        borderBottom={1}
        borderBottomColor="washedGray"
        width="100%"
      />
  </>
  );
}

function BlankParticipant({ length }) {
  const height = [`${length * 97}px`, `${length * 61}px`];
  return (
    <Box width="100%" height={height} />
  );
}
