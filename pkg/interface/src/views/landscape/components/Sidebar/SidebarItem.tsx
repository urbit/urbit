import _ from 'lodash';
import React, { MouseEvent, ReactNode } from 'react';
import urbitOb from 'urbit-ob';
import { Link, useHistory } from 'react-router-dom';
import { Icon, Row, Box, Text, BaseImage } from '@tlon/indigo-react';
import { Association, cite, deSig } from '@urbit/api';
import { HoverBoxLink } from '~/views/components/HoverBox';
import { Sigil } from '~/logic/lib/sigil';
import { Workspace } from '~/types/workspace';
import useContactState, { useContact } from '~/logic/state/contact';
import { getItemTitle, getModuleIcon, uxToHex } from '~/logic/lib/util';
import useGroupState from '~/logic/state/group';
import { useHarkDm, useHarkStat } from '~/logic/state/hark';
import useSettingsState from '~/logic/state/settings';
import useGraphState from '~/logic/state/graph';
import { usePreview } from '~/logic/state/metadata';

function useAssociationStatus(resource: string) {
  const [, , ship, name] = resource.split('/');
  const graphKey = `${deSig(ship)}/${name}`;
  const isSubscribed = useGraphState(s => s.graphKeys.has(graphKey));
  const stats = useHarkStat(`/graph/~${graphKey}`);
  const { count, each } = stats;
  const hasNotifications = false;
  const hasUnread = count > 0 || each.length > 0;
  if(!isSubscribed) {
    return 'unsubscribed';
  } else if (hasNotifications) {
    return 'notification';
  } else if (hasUnread) {
    return 'unread';
  } else {
    return undefined;
  }
}

export function SidebarItemBase(props: {
  to: string;
  selected: boolean;
  groupSelected?: boolean;
  hasNotification: boolean;
  hasUnread: boolean;
  unreadCount?: number;
  isSynced?: boolean;
  children?: ReactNode;
  title: string | ReactNode;
  mono?: boolean;
  fontSize?: string;
  pending?: boolean;
  isGroup?: boolean;
  indent?: number;
  onClick?: (e: MouseEvent) => void;
}) {
  const {
    title,
    children = null,
    to,
    selected,
    groupSelected = false,
    fontSize,
    hasNotification,
    hasUnread,
    unreadCount = 0,
    isSynced = false,
    mono = false,
    pending = false,
    isGroup = false,
    indent = 0,
    onClick
  } = props;
  const history = useHistory();
  const color = isSynced
    ? selected || (!isGroup && hasUnread)
      ? 'black'
      : 'gray'
    : 'lightGray';

  const hasGroupUnread = isGroup && (hasUnread || hasNotification);
  const hasChannelUnread = !isGroup && (hasUnread || hasNotification);

  const fontStyle = hasGroupUnread ? 'italic' : 'normal';
  const fontWeight = hasChannelUnread ? 600 : 'normal';
  const bg = pending ? 'lightBlue' : groupSelected ? 'washedGray' : 'white';
  const borderBottom = selected ? '1px solid lightGray' : undefined;

  return (
    <HoverBoxLink
      // ref={anchorRef}
      to={to}
      onClick={onClick}
      bg={bg}
      bgActive={pending ? 'washedBlue' : 'washedGray'}
      width="100%"
      display="flex"
      justifyContent="space-between"
      alignItems="center"
      position="relative"
      py={1}
      pl={`${8 + indent * 28}px`}
      pr={3}
      selected={selected}
    >
      <Row width="100%" alignItems="center" flex="1 auto" minWidth="0">
        {children}
        <Row
          width="100%"
          flexShrink={2}
          ml={2}
          display="flex"
          overflow="hidden"
          alignItems="center"
        >
          <Text
            lineHeight="tall"
            display="inline-block"
            overflow="hidden"
            mono={mono}
            color={color}
            fontSize={fontSize}
            fontWeight={fontWeight}
            fontStyle={fontStyle}
            style={{ textOverflow: 'ellipsis', whiteSpace: 'pre' }}
            className={selected ? '' : 'hover-underline'}
            borderBottom={borderBottom}
          >
            {title}
          </Text>
          {title === 'Messages' && (
            <Box style={{ display: 'inline-block', margin: '4px 0 0 16px' }} onClick={(e) => {
              e.preventDefault();
              e.stopPropagation();
              history.push('/~landscape/messages/new')
            }}>
              <Icon icon="Plus" color="gray" pr='12px' />
            </Box>
          )}
          {hasNotification && (
            <Box>
              <Icon icon="Bullet" color="blue" />
            </Box>
          )}
          {hasUnread && (
            <Box
              px="3px"
              py="1px"
              fontSize="10px"
              minWidth="fit-content"
              borderRadius="6px"
              color="white"
              backgroundColor="darkGray"
              textAlign="center"
              ml="6px"
              mr="6px"
            >
              {unreadCount}
            </Box>
          )}
        </Row>
      </Row>
    </HoverBoxLink>
  );
}

export const SidebarPendingItem = (props: {
  path: string;
  selected: boolean;
  indent?: number;
}) => {
  const { path, selected } = props;
  const { preview, error } = usePreview(path);
  const color = `#${uxToHex(preview?.metadata?.color || "0x0")}`;
  const title = preview?.metadata?.title || path;
  const to = `/~landscape/messages/pending/${path.slice(6)}`;
  return (
    <SidebarItemBase
      to={to}
      title={title}
      selected={selected}
      hasNotification={false}
      hasUnread={false}
      pending
      indent={props.indent}
    >
      <Box
        flexShrink={0}
        height={16}
        width={16}
        borderRadius={2}
        backgroundColor={color}
      />
    </SidebarItemBase>
  );
};

export const SidebarDmItem = React.memo(
  (props: {
    ship: string;
    selected?: boolean;
    workspace: Workspace;
    pending?: boolean;
    indent?: number;
  }) => {
    const { ship, selected = false, pending = false } = props;
    const contact = useContact(ship);
    const { hideAvatars, hideNicknames } = useSettingsState((s) => s.calm);
    const title =
      !hideNicknames && contact?.nickname
        ? contact?.nickname
        : cite(ship) ?? ship;
    const { count, each } = useHarkDm(ship);
    const unreads = count + each.length;
    const img =
      contact?.avatar && !hideAvatars ? (
        <BaseImage
          referrerPolicy="no-referrer"
          src={contact.avatar}
          width="16px"
          height="16px"
          borderRadius={2}
        />
      ) : (
        <Sigil
          ship={ship}
          color={`#${uxToHex(contact?.color || "0x0")}`}
          icon
          padding={2}
          size={16}
        />
      );

    return (
      <SidebarItemBase
        selected={selected}
        hasNotification={unreads > 0}
        hasUnread={unreads > 0}
        to={`/~landscape/messages/dm/${ship}`}
        title={title}
        mono={hideAvatars || !contact?.nickname}
        isSynced
        pending={pending}
        unreadCount={unreads}
        indent={props.indent}
      >
        {img}
      </SidebarItemBase>
    );
  }
);
// eslint-disable-next-line max-lines-per-function
export const SidebarAssociationItem = React.memo(
  (props: {
    hideUnjoined: boolean;
    association: Association;
    selected: boolean;
    workspace: Workspace;
    groupSelected?: boolean;
    fontSize?: string;
    unreadCount?: number;
    hasNotification?: boolean;
    indent?: number;
  }) => {
    const { association, selected } = props;
    const history = useHistory();
    const title = association ? getItemTitle(association) || "" : "";
    const appName = association?.["app-name"];
    let mod: string = appName;
    if (
      association?.metadata?.config &&
      "graph" in association.metadata.config
    ) {
      mod = association.metadata.config.graph;
    }
    const pending = useGroupState(s => association.group in s.pendingJoin);
    const rid = association?.resource;
    const { hideNicknames } = useSettingsState((s) => s.calm);
    const contacts = useContactState((s) => s.contacts);
    const group = useGroupState(s => association ? s.groups[association.group] : undefined);
    const isUnmanaged = group?.hidden || false;
    const DM = isUnmanaged && props.workspace?.type === "messages";
    const itemStatus = useAssociationStatus(rid);
    const hasUnread = itemStatus === "unread";
    const isSynced = itemStatus !== "unsubscribed";
    let baseUrl = `/~landscape${association.group}`;

    if (DM) {
      baseUrl = "/~landscape/messages";
    } else if (isUnmanaged) {
      baseUrl = "/~landscape/home";
    }

    const to = isSynced
      ? `${baseUrl}/resource/${mod}${rid}`
      : `${baseUrl}/join/${mod}${rid}`;

    const onClick = pending ? () => {
      useGroupState.getState().doneJoin(rid);
    } : () => {
      if (group && !history.location.pathname.includes(baseUrl)) {
        history.push(baseUrl);
      }
    };

    if (props.hideUnjoined && !isSynced) {
      return null;
    }

    const participantNames = (str: string) => {
      const color = isSynced
        ? hasUnread || props.hasNotification
          ? 'black'
          : 'gray'
        : 'lightGray';
      if (_.includes(str, ',') && _.startsWith(str, '~')) {
        const names = _.split(str, ', ');
        return names.map((name, idx) => {
          if (urbitOb.isValidPatp(name)) {
            if (contacts[name]?.nickname && !hideNicknames)
              return (
                <Text key={name} bold={hasUnread} color={color}>
                  {contacts[name]?.nickname}
                  {idx + 1 != names.length ? ', ' : null}
                </Text>
              );
            return (
              <Text key={name} mono bold={hasUnread} color={color}>
                {name}
                <Text color={color}>
                  {idx + 1 != names.length ? ', ' : null}
                </Text>
              </Text>
            );
          } else {
            return name;
          }
        });
      } else {
        return str;
      }
    };

    return (
      <SidebarItemBase
        to={to}
        selected={selected}
        groupSelected={props.groupSelected}
        hasUnread={hasUnread}
        fontSize={props.fontSize}
        isSynced={isSynced}
        title={
          DM && !urbitOb.isValidPatp(title) ? participantNames(title) : title
        }
        hasNotification={props.hasNotification}
        pending={pending}
        onClick={onClick}
        unreadCount={props.unreadCount}
        indent={props.indent}
      >
        {DM ? (
          <Box
            flexShrink={0}
            height={16}
            width={16}
            borderRadius={2}
            backgroundColor={
              `#${uxToHex(props?.association?.metadata?.color)}` || '#000000'
            }
          />
        ) : (
          <Icon
            display="block"
            color={isSynced ? 'black' : 'lightGray'}
            icon={getModuleIcon(mod as any)}
          />
        )}
      </SidebarItemBase>
    );
  }
);
