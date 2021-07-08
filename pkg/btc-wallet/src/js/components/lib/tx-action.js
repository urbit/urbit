import React from 'react';
import { Box, Icon, Row, Text, LoadingSpinner } from '@tlon/indigo-react';
import { useSettings } from '../../hooks/useSettings';

const TxAction = ({ action, pending, txid }) => {
  const { network } = useSettings();
  const leftIcon =
    action === 'sent'
      ? 'ArrowSouth'
      : action === 'recv'
      ? 'ArrowNorth'
      : action === 'fail'
      ? 'X'
      : 'NullIcon';

  const actionColor =
    action === 'sent'
      ? 'sentBlue'
      : action === 'recv'
      ? 'recvGreen'
      : action === 'fail'
      ? 'gray'
      : 'red';

  const actionText =
    action === 'sent' && !pending
      ? 'Sent BTC'
      : action === 'sent' && pending
      ? 'Sending BTC'
      : action === 'recv' && !pending
      ? 'Received BTC'
      : action === 'recv' && pending
      ? 'Receiving BTC'
      : action === 'fail'
      ? 'Failed'
      : 'error';

  const pendingSpinner = !pending ? null : (
    <LoadingSpinner background="midOrange" foreground="orange" />
  );

  const url =
    network === 'testnet'
      ? `http://blockstream.info/testnet/tx/${txid}`
      : `http://blockstream.info/tx/${txid}`;

  return (
    <Row alignItems="center">
      <Box
        backgroundColor={actionColor}
        width="24px"
        height="24px"
        textAlign="center"
        alignItems="center"
        borderRadius="2px"
        mr={2}
        p={1}
      >
        <Icon icon={leftIcon} color="white" />
      </Box>
      <Text color={actionColor} fontSize="14px">
        {actionText}
      </Text>
      <a href={url} target="_blank" rel="noreferrer">
        <Icon color={actionColor} icon="ArrowNorthEast" ml={1} mr={2} />
      </a>
      {pendingSpinner}
    </Row>
  );
};

export default TxAction;
