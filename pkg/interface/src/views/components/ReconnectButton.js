import React from 'react';
import { Box, Text } from '@tlon/indigo-react';

const ReconnectButton = ({ connection, subscription }) => {
  const connectedStatus = connection || 'connected';
  const reconnect = subscription.restart.bind(subscription);
  if (connectedStatus === 'disconnected') {
    return (
      <>
      <Box
        ml={2}
        px={2}
        py={1}
        display='inline-block'
        color='red'
        bg="white"
        border={1}
        lineHeight='min'
        borderRadius={2}
        style={{ cursor: 'pointer' }}
        onClick={reconnect}>
        <Text color='red'>Reconnect ↻</Text>
      </Box>
      </>
    );
    } else if (connectedStatus === 'reconnecting') {
      return (
        <>
        <Box
          ml={2}
          px={2}
          py={1}
          lineHeight="min"
          bg='white'
          display='inline-block'
          color='yellow'
          border={1}
          borderRadius={2}>
          <Text color='yellow'>Reconnecting</Text>
        </Box>
        </>
      );
      } else {
        return null;
      }
    };

export default ReconnectButton;
