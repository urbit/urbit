import React, { useCallback } from 'react';
import { Box, Col, Row, Text } from '@tlon/indigo-react';
import { StatelessAsyncAction } from '~/views/components/StatelessAsyncAction';
import Author from '~/views/components/Author';
import GlobalApi from '~/logic/api/global';
import { useHistory } from 'react-router';

export function PendingDm(props: { ship: string; api: GlobalApi }) {
  const { ship, api } = props;
  const { push } = useHistory();
  const onAccept = useCallback(async () => {
    await api.graph.acceptDm(ship);
    push(`/~landscape/messages/dm/${ship}`);
  }, [ship, push, api]);

  const onDecline = useCallback(async () => {
    await api.graph.declineDm(ship);
  }, [ship, api]);

  return (
    <Box
      display="grid"
      bg="washedGray"
      borderRadius="2"
      gridTemplateColumns={['1fr 24px', '1fr 200px']}
      gridTemplateRows="auto"
      gridTemplateAreas="'header actions' 'main main'"
      p={2}
      m={2}
    >
      <Row alignItems="center" gridArea="header">
        <Author ship={ship} />
        <Text ml="1" lineHeight="tall">
          invited you to a DM
        </Text>
      </Row>
      <Row
        alignItems="center"
        gapX="2"
        gridArea="actions"
        justifyContent="flex-end"
      >
        <StatelessAsyncAction height="auto" primary p="1" onClick={onAccept}>
          Accept
        </StatelessAsyncAction>
        <StatelessAsyncAction
          height="auto"
          destructive
          p="1"
          onClick={onDecline}
        >
          Decline
        </StatelessAsyncAction>
      </Row>
    </Box>
  );
}
