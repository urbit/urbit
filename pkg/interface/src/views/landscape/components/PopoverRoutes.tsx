import { Box, Col, Text } from '@tlon/indigo-react';
import { Association, deSig, Group } from '@urbit/api';
import React, { ReactElement, useCallback, useRef } from 'react';
import { Link, Route, RouteComponentProps, Switch } from 'react-router-dom';
import { resourceFromPath } from '~/logic/lib/group';
import { useHashLink } from '~/logic/lib/useHashLink';
import { ModalOverlay } from '~/views/components/ModalOverlay';
import { SidebarItem } from '~/views/landscape/components/SidebarItem';
import { DeleteGroup } from './DeleteGroup';
import { GroupSettings } from './GroupSettings/GroupSettings';
import { Participants } from './Participants';

export function PopoverRoutes(
  props: {
    baseUrl: string;
    group: Group;
    association: Association;
  } & RouteComponentProps
): ReactElement {
  const relativeUrl = (url: string) => `${props.baseUrl}/popover${url}`;

  const onDismiss = useCallback(() => {
    props.history.push(props.baseUrl);
  }, [props.history.push, props.baseUrl]);

  useHashLink();

  const groupSize = props.group.members.size;
  const ship = resourceFromPath(props.association?.group ?? '/ship/~zod/group').ship;
  const owner = deSig(ship) === window.ship;

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
                  <Text my={4} mx={3} fontWeight="600" fontSize={2}>Group Settings</Text>
                  <Col gapY={2}>
                    <Text my={1} mx={3} gray>Group</Text>
                    <SidebarItem
                      icon='Notifications'
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
                        <Box pt={3} mb={1} mx={3}>
                          <Text gray>Administration</Text>
                        </Box>
                        <SidebarItem
                          icon="Groups"
                          to={relativeUrl('/settings#group-details')}
                          text="Group Details"
                        />
                        <SidebarItem
                          icon="Dashboard"
                          to={relativeUrl('/settings#channels')}
                          text="Channel Management"
                        />
                        { owner && (
                        <SidebarItem
                          icon="Server"
                          to={relativeUrl('/settings#feed')}
                          text="Group Feed"
                        />)}

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
