import React, { useRef, useCallback, ReactElement } from 'react';
import { Route, Switch, RouteComponentProps, Link, useHistory } from 'react-router-dom';

import { Box,  Col, Text } from '@tlon/indigo-react';
import {
  GroupNotificationsConfig,
  Associations,
  Contacts,
  Contact,
  Group,
  Association,
  resourceFromPath
} from '@urbit/api';

import GlobalApi from '~/logic/api/global';
import { GroupSettings } from './GroupSettings/GroupSettings';
import { Participants } from './Participants';
import { useHashLink } from '~/logic/lib/useHashLink';
import { DeleteGroup } from './DeleteGroup';
import { ModalOverlay } from '~/views/components/ModalOverlay';
import { SidebarItem } from '~/views/landscape/components/SidebarItem';
import { S3State } from '~/types';

export function PopoverRoutes(
  props: {
    baseUrl: string;
    group: Group;
    association: Association;
    s3: S3State;
    notificationsGroupConfig: GroupNotificationsConfig;
    rootIdentity: Contact;
  } & RouteComponentProps
): ReactElement {
  const relativeUrl = (url: string) => `${props.baseUrl}/popover${url}`;
  const innerRef = useRef(null);
  const history = useHistory();

  const onDismiss = useCallback(() => {
    history.push(props.baseUrl);
  }, [history.push, props.baseUrl]);

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
                    <DeleteGroup owner={owner} association={props.association} />
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
                      notificationsGroupConfig={props.notificationsGroupConfig}
                      s3={props.s3}
                    />
                  )}
                  {view === 'participants' && (
                    <Participants
                      group={props.group}
                      association={props.association}
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
