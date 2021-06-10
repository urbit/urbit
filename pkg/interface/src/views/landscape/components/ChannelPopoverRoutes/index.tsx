import { Box, Col, Row, Text } from '@tlon/indigo-react';
import {
    Association,

  deleteGraph,

    Group,
    leaveGraph,
    metadataRemove
} from '@urbit/api';
import React, { useCallback, useRef } from 'react';
import { Link, useHistory } from 'react-router-dom';
import { isChannelAdmin, isHost } from '~/logic/lib/group';
import { useHashLink } from '~/logic/lib/useHashLink';
import { FormGroup } from '~/views/components/FormGroup';
import { ModalOverlay } from '~/views/components/ModalOverlay';
import { StatelessAsyncButton } from '~/views/components/StatelessAsyncButton';
import { GraphPermissions } from './ChannelPermissions';
import { ChannelDetails } from './Details';
import { ChannelNotifications } from './Notifications';
import { ChannelPopoverRoutesSidebar } from './Sidebar';
import airlock from '~/logic/api';

interface ChannelPopoverRoutesProps {
  baseUrl: string;
  rootUrl: string;
  association: Association;
  group: Group;
}

export function ChannelPopoverRoutes(props: ChannelPopoverRoutesProps) {
  const { association, group } = props;
  useHashLink();
  const overlayRef = useRef<HTMLElement>();
  const history = useHistory();

  const onDismiss = useCallback(() => {
    history.push(props.baseUrl);
  }, [history, props.baseUrl]);

  const handleUnsubscribe = async () => {
    const [,,ship,name] = association.resource.split('/');
    await airlock.thread(leaveGraph(ship, name));
    history.push(props.rootUrl);
  };
  const handleRemove = async () => {
    await airlock.poke(metadataRemove('graph', association.resource, association.group));
    history.push(props.rootUrl);
  };
  const handleArchive = async () => {
    const [,,,name] = association.resource.split('/');

    airlock.thread(deleteGraph(`~${window.ship}`, name));
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
        border={1}
        borderColor="lightGray"
        borderRadius={2}
        bg="white"
        height="100%"
      >
        <Box pt={4} px={4} display={['block', 'none']}>
          <Link to={props.baseUrl}>
            <Text fontSize={1}>{'<- Back'}</Text>
          </Link>
        </Box>
        <ChannelPopoverRoutesSidebar
          isAdmin={canAdmin}
          isOwner={isOwner}
          baseUrl={props.baseUrl}
        />
        <FormGroup onReset={onDismiss} height="100%" overflowY="auto" pt={5} flexGrow={1}>
          <ChannelNotifications {...props} />
          {!isOwner && (
            <Col mx={4} mb={6} flexShrink={0}>
              <Text id="unsubscribe" fontSize={2} fontWeight="bold">
                Unsubscribe from Channel
              </Text>
              <Text mt={1} maxWidth="450px" gray>
                Unsubscribing from a channel will revoke your ability to read
                its contents. Any permissions set by the channel host will still
                apply once you have left.
              </Text>
              <Row mt={3}>
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
              <Col mx={4} mt={5} mb={6} flexShrink={0}>
                <Text id="archive" fontSize={2} fontWeight="bold">
                  Archive channel
                </Text>
                <Text mt={1} maxWidth="450px" gray>
                  Archiving a channel will prevent further updates to the channel.
                  Users who are currently joined to the channel will retain a copy
                  of the channel.
                </Text>
                <Row mt={3}>
                  <StatelessAsyncButton destructive onClick={handleArchive}>
                    Archive {props.association.metadata.title}
                  </StatelessAsyncButton>
                </Row>
              </Col>

              ) : (
              <Col mx={4} my={6} flexShrink={0}>
              <Text id="remove" fontSize={2} fontWeight="bold">
                Remove channel from group
              </Text>
              <Text mt={1} maxWidth="450px" gray>
                Removing a channel will prevent further updates to the channel.
                Users who are currently joined to the channel will retain a copy
                of the channel.
              </Text>
              <Row mt={3}>
                <StatelessAsyncButton destructive onClick={handleRemove}>
                  Remove {props.association.metadata.title}
                </StatelessAsyncButton>
              </Row>
            </Col>

              )}
            </>
          )}
      </FormGroup>
      </Row>
    </ModalOverlay>
  );
}
