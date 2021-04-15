import React from 'react';
import { Text, Col } from '@tlon/indigo-react';
import { SidebarItem } from '../SidebarItem';

export function ChannelPopoverRoutesSidebar(props: {
  baseUrl: string;
  isOwner: boolean;
  isAdmin: boolean;
}) {
  const { baseUrl, isAdmin, isOwner } = props;

  const relativePath = (p: string) => `${baseUrl}${p}`;

  return (
    <Col
      display={['none', 'flex-column']}
      minWidth="200px"
      borderRight="1"
      borderRightColor="washedGray"
      py="5"
      gapY="2"
    >
      <Text mx="3" my="3" fontSize="1" fontWeight="medium">
        Channel Settings
      </Text>
      <Text mx="3" my="2" gray>
        Preferences
      </Text>
      <SidebarItem
        icon='Notifications'
        text="Notifications"
        to={relativePath('/settings#notifications')}
      />
      {!isOwner && (
        <SidebarItem
          icon="LogOut"
          text="Unsubscribe"
          color="red"
          to={relativePath('/settings#unsubscribe')}
        />
      )}
      {isAdmin && (
        <>
          <Text mx="3" py="2" gray>
            Administration
          </Text>
          <SidebarItem
            icon="BootNode"
            text="Channel Details"
            to={relativePath('/settings#details')}
          />
          <SidebarItem
            icon="Keyfile"
            text="Permissions"
            to={relativePath('/settings#permissions')}
          />
          { isOwner ? (
            <SidebarItem
              icon="X"
              text="Archive Channel"
              to={relativePath('/settings#archive')}
              color="red"
            />
          ) : (
            <SidebarItem
              icon="X"
              text="Archive Channel"
              to={relativePath('/settings#remove')}
              color="red"
            />
          )}
        </>
      )}
    </Col>
  );
}
