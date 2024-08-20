import _ from 'lodash';
import React, { ReactNode } from 'react';
import urbitOb from 'urbit-ob';
import { Icon, Row, Box, Text, BaseImage } from '@tlon/indigo-react';
import { Association, cite, deSig } from '@urbit/api';
import { HoverBoxLink } from '~/views/components/HoverBox';
import { Sigil } from '~/logic/lib/sigil';
import { Workspace } from '~/types/workspace';
import useContactState, { useContact } from '~/logic/state/contact';
import { getItemTitle, getModuleIcon, uxToHex } from '~/logic/lib/util';
import useGroupState from '~/logic/state/group';
import Dot from '~/views/components/Dot';
import { useHarkDm, useHarkStat } from '~/logic/state/hark';
import useSettingsState from '~/logic/state/settings';
import useGraphState from '~/logic/state/graph';
import {usePreview} from '~/logic/state/metadata';

function useAssociationStatus(resource: string) {
  const [, , ship, name] = resource.split("/");
  const graphKey = `${deSig(ship)}/${name}`;
  const isSubscribed = useGraphState((s) => s.graphKeys.has(graphKey));
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

function SidebarItemBase(props: {
  to: string;
  selected: boolean;
  hasNotification: boolean;
  hasUnread: boolean;
  isSynced?: boolean;
  children: ReactNode;
  title: string | ReactNode;
  mono?: boolean;
  pending?: boolean;
  onClick?: () => void;
}) {
  const {
    title,
    children,
    to,
    selected,
    hasNotification,
    hasUnread,
    isSynced = false,
    mono = false,
    pending = false,
    onClick
  } = props;
  const color = isSynced
    ? hasUnread || hasNotification
      ? "black"
      : "gray"
    : "lightGray";

  const fontWeight = hasUnread || hasNotification ? "500" : "normal";

  return (
    <HoverBoxLink
      // ref={anchorRef}
      to={to}
      onClick={onClick}
      bg={pending ? "lightBlue" : "white"}
      bgActive={pending ? "washedBlue" : "washedGray"}
      width="100%"
      display="flex"
      justifyContent="space-between"
      alignItems="center"
      py={1}
      pl={3}
      pr={3}
      selected={selected}
    >
      <Row width="100%" alignItems="center" flex="1 auto" minWidth="0">
        {hasNotification && (
          <Text
            color="black"
            marginLeft={-2}
            width={2}
            display="flex"
            alignItems="center"
          >
            <Dot />
          </Text>
        )}
        {children}

        <Box
          width="100%"
          flexShrink={2}
          ml={2}
          display="flex"
          overflow="hidden"
        >
          <Text
            lineHeight="tall"
            display="inline-block"
            flex="1"
            overflow="hidden"
            width="100%"
            mono={mono}
            color={color}
            fontWeight={fontWeight}
            style={{ textOverflow: "ellipsis", whiteSpace: "pre" }}
          >
            {title}
          </Text>
        </Box>
      </Row>
    </HoverBoxLink>
  );
}

export const SidebarPendingItem = (props: {
  path: string;
  selected: boolean;
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
}

export const SidebarDmItem = React.memo(
  (props: {
    ship: string;
    selected?: boolean;
    workspace: Workspace;
    pending?: boolean;
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
        hasNotification={false}
        hasUnread={(unreads as number) > 0}
        to={`/~landscape/messages/dm/${ship}`}
        title={title}
        mono={hideAvatars || !contact?.nickname}
        isSynced
        pending={pending}
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
  }) => {
    const { association, selected } = props;
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
    const hasNotification = itemStatus === "notification";
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
    } : undefined;

    if (props.hideUnjoined && !isSynced) {
      return null;
    }

    const participantNames = (str: string) => {
      const color = isSynced
        ? hasUnread || hasNotification
          ? "black"
          : "gray"
        : "lightGray";
      if (_.includes(str, ",") && _.startsWith(str, "~")) {
        const names = _.split(str, ", ");
        return names.map((name, idx) => {
          if (urbitOb.isValidPatp(name)) {
            if (contacts[name]?.nickname && !hideNicknames)
              return (
                <Text key={name} bold={hasUnread} color={color}>
                  {contacts[name]?.nickname}
                  {idx + 1 != names.length ? ", " : null}
                </Text>
              );
            return (
              <Text key={name} mono bold={hasUnread} color={color}>
                {name}
                <Text color={color}>
                  {idx + 1 != names.length ? ", " : null}
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
        hasUnread={hasUnread}
        isSynced={isSynced}
        title={
          DM && !urbitOb.isValidPatp(title) ? participantNames(title) : title
        }
        hasNotification={hasNotification}
        pending={pending}
        onClick={onClick}
      >
        {DM ? (
          <Box
            flexShrink={0}
            height={16}
            width={16}
            borderRadius={2}
            backgroundColor={
              `#${uxToHex(props?.association?.metadata?.color)}` || "#000000"
            }
          />
        ) : (
          <Icon
            display="block"
            color={isSynced ? "black" : "lightGray"}
            icon={getModuleIcon(mod as any)}
          />
        )}
      </SidebarItemBase>
    );
  }
);
