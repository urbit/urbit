import React, { useCallback } from 'react';
import { Box, Row, Text } from '@tlon/indigo-react';
import { StatelessAsyncAction } from '~/views/components/StatelessAsyncAction';
import Author from '~/views/components/Author';
import { useHistory } from 'react-router';
import { acceptDm, declineDm } from '@urbit/api/graph';
import airlock from '~/logic/api';

export function PendingDm(props: { ship: string; }) {
  const { ship } = props;
  const { push } = useHistory();
  const onAccept = useCallback(async () => {
    await airlock.poke(acceptDm(ship));
    push(`/~landscape/messages/dm/${ship}`);
  }, [ship, push]);

  const onDecline = useCallback(async () => {
    await airlock.poke(declineDm(ship));
  }, [ship]);

  return (
    <Box
      display="grid"
      bg="washedGray"
      borderRadius="2"
      gridTemplateColumns={['1fr 24px', '1fr 200px']}
      gridRowGap={2}
      gridTemplateRows="auto"
      gridTemplateAreas={[
        '\'header header\' \'actions actions\'',
        '\'header actions\' \'main main\''
      ]}
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
        <StatelessAsyncAction
          backgroundColor="white"
          height="auto"
          primary
          p="1"
          onClick={onAccept}
        >
          Accept
        </StatelessAsyncAction>
        <StatelessAsyncAction
          backgroundColor="white"
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
