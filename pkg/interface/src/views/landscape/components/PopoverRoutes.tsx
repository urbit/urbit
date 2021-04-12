import React, { useRef, useCallback, ReactElement } from 'react';
import { Route, Switch, RouteComponentProps, Link } from 'react-router-dom';
import { Box,  Col, Text } from '@tlon/indigo-react';

import { GroupNotificationsConfig, Associations } from '@urbit/api';
import { Contacts, Contact } from '@urbit/api/contacts';
import { Group } from '@urbit/api/groups';
import { Association } from '@urbit/api/metadata';

import GlobalApi from '~/logic/api/global';
import { GroupSettings } from './GroupSettings/GroupSettings';
import { Participants } from './Participants';
import { useHashLink } from '~/logic/lib/useHashLink';
import { DeleteGroup } from './DeleteGroup';
import { resourceFromPath } from '~/logic/lib/group';
import { ModalOverlay } from '~/views/components/ModalOverlay';
import { SidebarItem } from '~/views/landscape/components/SidebarItem';
import { StorageState } from '~/types';

export function PopoverRoutes(
  props: {
    baseUrl: string;
    group: Group;
    association: Association;
    api: GlobalApi;
    notificationsGroupConfig: GroupNotificationsConfig;
    rootIdentity: Contact;
  } & RouteComponentProps
): ReactElement {
  const relativeUrl = (url: string) => `${props.baseUrl}/popover${url}`;
  const innerRef = useRef(null);

  const onDismiss = useCallback(() => {
    props.history.push(props.baseUrl);
  }, [props.history.push, props.baseUrl]);

  useHashLink();

  const groupSize = props.group.members.size;

  const owner = resourceFromPath(props.association.group).ship.slice(1) === window.ship;

  const admin = props.group?.tags?.role?.admin.has(window.ship) || false;

  return (
    <Switch>
      <Route
        path={[relativeUrl('/:view'), relativeUrl('')]}
        render={(routeProps) => {
          const { view } = routeProps.match.params;
          return (
            <ModalOverlay
              spacing={[3,5,7]}
              ref={innerRef}
              border={1}
              borderColor="washedGray"
              borderRadius={1}
              width="100%"
              height="100%"
              bg="white"
              dismiss={onDismiss}
            >
              <Box
                display="grid"
                gridTemplateRows={['32px 1fr', '100%']}
                gridTemplateColumns={['100%', '250px 1fr']}
                height="100%"
                width="100%"
              >
                <Col
                  display={view ? ['none', 'flex'] : 'flex'}
                  borderRight={1}
                  borderRightColor="washedGray"
                >
                  <Text my="4" mx="3" fontWeight="600" fontSize="2">Group Settings</Text>
                  <Col gapY="2">
                    <Text my="1" mx="3" gray>Group</Text>
                    <SidebarItem
                      icon="Inbox"
                      to={relativeUrl('/settings#notifications')}
                      text="Notifications"
                    />
                    <SidebarItem
                      icon="Users"
                      to={relativeUrl('/participants')}
                      text="Participants"
                      selected={view === 'participants'}
                    ><Text gray>{groupSize}</Text>
                    </SidebarItem>
                    { admin && (
                      <>
                        <Box pt="3" mb="1" mx="3">
                          <Text gray>Administration</Text>
                        </Box>
                        <SidebarItem
                          icon="Groups"
                          to={relativeUrl('/settings#group-details')}
                          text="Group Details"
                        />
                        <SidebarItem
                          icon="Spaces"
                          to={relativeUrl('/settings#channels')}
                          text="Channel Management"
                        />
                      </>
                    )}
                    <DeleteGroup owner={owner} api={props.api} association={props.association} />
                  </Col>
                </Col>
                <Box
                  gridArea={'1 / 1 / 2 / 2'}
                  p={2}
                  display={['auto', 'none']}
                >
                  <Link to={view ? relativeUrl('') : props.baseUrl}>
                    <Text>{'<- Back'}</Text>
                  </Link>
                </Box>
                <Box overflow="hidden">
                  {view === 'settings' && (
                    <GroupSettings
                      baseUrl={`${props.baseUrl}/popover`}
                      group={props.group}
                      association={props.association}
                      api={props.api}
                    />
                  )}
                  {view === 'participants' && (
                    <Participants
                      group={props.group}
                      association={props.association}
                      api={props.api}
                    />
                  )}
                </Box>
              </Box>
            </ModalOverlay>
          );
        }}
      />
    </Switch>
  );
}
