import React, { useRef, useCallback } from 'react';

import { Col, Box, Text, Row } from '@tlon/indigo-react';
import {
  Association,
  Groups,
  Group,
  Rolodex,
  NotificationGraphConfig
} from '@urbit/api';

import { ModalOverlay } from '~/views/components/ModalOverlay';
import { GraphPermissions } from './ChannelPermissions';
import { ChannelPopoverRoutesSidebar } from './Sidebar';
import { ChannelDetails } from './Details';
import GlobalApi from '~/logic/api/global';
import { useHashLink } from '~/logic/lib/useHashLink';
import { useHistory, Link } from 'react-router-dom';
import { ChannelNotifications } from './Notifications';
import { StatelessAsyncButton } from '~/views/components/StatelessAsyncButton';
import { isChannelAdmin, isHost } from '~/logic/lib/group';

interface ChannelPopoverRoutesProps {
  baseUrl: string;
  rootUrl: string;
  association: Association;
  group: Group;
  api: GlobalApi;
}

export function ChannelPopoverRoutes(props: ChannelPopoverRoutesProps) {
  const { association, group, api } = props;
  useHashLink();
  const overlayRef = useRef<HTMLElement>();
  const history = useHistory();

  const onDismiss = useCallback(() => {
    history.push(props.baseUrl);
  }, [history, props.baseUrl]);

  const handleUnsubscribe = async () => {
    const [,,ship,name] = association.resource.split('/');
    await api.graph.leaveGraph(ship, name);
    history.push(props.rootUrl);
  };
  const handleRemove = async () => {
    await api.metadata.remove('graph', association.resource, association.group);
    history.push(props.rootUrl);
  };
  const handleArchive = async () => {
    const [,,,name] = association.resource.split('/');
    api.graph.deleteGraph(name);
    return history.push(props.rootUrl);
  };

  const canAdmin = isChannelAdmin(group, association.resource);
  const isOwner = isHost(association.resource);

  return (
    <ModalOverlay
      bg="transparent"
      height="100%"
      width="100%"
      spacing={[3, 5, 7]}
      ref={overlayRef}
      dismiss={onDismiss}
    >
      <Row
        flexDirection={['column', 'row']}
        border="1"
        borderColor="lightGray"
        borderRadius="2"
        bg="white"
        height="100%"
      >
        <Box pt="4" px="4" display={['block', 'none']}>
          <Link to={props.baseUrl}>
            <Text fontSize="1">{'<- Back'}</Text>
          </Link>
        </Box>
        <ChannelPopoverRoutesSidebar
          isAdmin={canAdmin}
          isOwner={isOwner}
          baseUrl={props.baseUrl}
        />
        <Col height="100%" overflowY="auto" p="5" flexGrow={1}>
          <ChannelNotifications {...props} />
          {!isOwner && (
            <Col mb="6" flexShrink={0}>
              <Text id="unsubscribe" fontSize="2" fontWeight="bold">
                Unsubscribe from Channel
              </Text>
              <Text mt="1" maxWidth="450px" gray>
                Unsubscribing from a channel will revoke your ability to read
                its contents. Any permissions set by the channel host will still
                apply once you have left.
              </Text>
              <Row mt="3">
                <StatelessAsyncButton destructive onClick={handleUnsubscribe}>
                  Unsubscribe from {props.association.metadata.title}
                </StatelessAsyncButton>
              </Row>
            </Col>
          )}
          {canAdmin && (
            <>
              <ChannelDetails {...props} />
              <GraphPermissions {...props} />
              { isOwner ? (
              <Col mt="5" mb="6" flexShrink={0}>
                <Text id="archive" fontSize="2" fontWeight="bold">
                  Archive channel
                </Text>
                <Text mt="1" maxWidth="450px" gray>
                  Archiving a channel will prevent further updates to the channel.
                  Users who are currently joined to the channel will retain a copy
                  of the channel.
                </Text>
                <Row mt="3">
                  <StatelessAsyncButton destructive onClick={handleArchive}>
                    Archive {props.association.metadata.title}
                  </StatelessAsyncButton>
                </Row>
              </Col>

              ) : (
              <Col mt="5" mb="6" flexShrink={0}>
              <Text id="remove" fontSize="2" fontWeight="bold">
                Remove channel from group
              </Text>
              <Text mt="1" maxWidth="450px" gray>
                Removing a channel will prevent further updates to the channel.
                Users who are currently joined to the channel will retain a copy
                of the channel.
              </Text>
              <Row mt="3">
                <StatelessAsyncButton destructive onClick={handleRemove}>
                  Remove {props.association.metadata.title}
                </StatelessAsyncButton>
              </Row>
            </Col>

              )}
            </>
          )}
        </Col>
      </Row>
    </ModalOverlay>
  );
}
