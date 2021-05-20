import React, { ReactElement, useRef, ReactNode } from 'react';
import urbitOb from 'urbit-ob';

import { Icon, Row, Box, Text, BaseImage } from '@tlon/indigo-react';
import { Groups, Association, Rolodex, cite } from '@urbit/api';

import { HoverBoxLink } from '~/views/components/HoverBox';
import { Sigil } from '~/logic/lib/sigil';
import { useTutorialModal } from '~/views/components/useTutorialModal';
import { TUTORIAL_HOST, TUTORIAL_GROUP } from '~/logic/lib/tutorialModal';
import { Workspace } from '~/types/workspace';
import { useContact } from '~/logic/state/contact';
import { getItemTitle, getModuleIcon, uxToHex } from '~/logic/lib/util';
import useGroupState from '~/logic/state/group';
import Dot from '~/views/components/Dot';
import { SidebarAppConfigs } from './types';

function SidebarItemBase(props: {
  to: string;
  selected: boolean;
  hasNotification: boolean;
  hasUnread: boolean;
  isSynced?: boolean;
  children: ReactNode;
  title: string;
  mono?: boolean;
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
  } = props;
  const color = isSynced ? 'black' : 'lightGray';

  const fontWeight = hasUnread || hasNotification ? '500' : 'normal';

  return (
    <HoverBoxLink
      //ref={anchorRef}
      to={to}
      bg="white"
      bgActive="washedGray"
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
            mono={urbitOb.isValidPatp(title)}
            color={color}
            fontWeight={fontWeight}
            style={{ textOverflow: 'ellipsis', whiteSpace: 'pre' }}
          >
            {title}
          </Text>
        </Box>
      </Row>
    </HoverBoxLink>
  );
}

export function SidebarDmItem(props: {
  ship: string;
  selected?: boolean;
  workspace: Workspace;
}) {
  const { ship, selected = false } = props;
  const contact = useContact(ship);
  const title = contact?.nickname || (cite(ship) ?? ship)
  const hideAvatars = false;
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
        color={`#${uxToHex(contact?.color || '0x0')}`}
        icon
        padding={2}
        size={16}
      />
    );

  return (
    <SidebarItemBase
      selected={selected}
      hasNotification={false}
      hasUnread={false}
      to={`/~landscape/messages/dm/${ship}`}
      title={title}
      mono={!!contact?.nickname}
      isSynced
    >
      {img}
    </SidebarItemBase>
  );
}
// eslint-disable-next-line max-lines-per-function
export function SidebarAssociationItem(props: {
  hideUnjoined: boolean;
  association: Association;
  path: string;
  selected: boolean;
  apps: SidebarAppConfigs;
  workspace: Workspace;
}) {
  const { association, path, selected, apps } = props;
  let title = getItemTitle(association) || '';
  const appName = association?.['app-name'];
  let mod = appName;
  if (association?.metadata?.config && 'graph' in association.metadata.config) {
    mod = association.metadata.config.graph;
  }
  const rid = association?.resource;
  const groupPath = association?.group;
  const groups = useGroupState((state) => state.groups);
  const anchorRef = useRef<HTMLAnchorElement>(null);
  useTutorialModal(
    mod as any,
    groupPath === `/ship/${TUTORIAL_HOST}/${TUTORIAL_GROUP}`,
    anchorRef
  );
  const app = apps[appName];
  const isUnmanaged = groups?.[groupPath]?.hidden || false;
  if (!app) {
    return null;
  }
  const DM = isUnmanaged && props.workspace?.type === 'messages';

  const itemStatus = app.getStatus(path);

  const hasNotification = itemStatus === 'notification';

  const hasUnread = itemStatus === 'unread';

  const isSynced = itemStatus !== 'unsubscribed';

  let baseUrl = `/~landscape${groupPath}`;

  if (DM) {
    baseUrl = '/~landscape/messages';
  } else if (isUnmanaged) {
    baseUrl = '/~landscape/home';
  }

  const to = isSynced
    ? `${baseUrl}/resource/${mod}${rid}`
    : `${baseUrl}/join/${mod}${rid}`;

  if (props.hideUnjoined && !isSynced) {
    return null;
  }

  return (
    <SidebarItemBase
      to={to}
      selected={selected}
      hasUnread={hasUnread}
      title={title}
      hasNotification={hasNotification}
    >
      <Icon
        display="block"
        color={isSynced ? 'black' : 'lightGray'}
        icon={getModuleIcon(mod)}
      />
    </SidebarItemBase>
  );
}
