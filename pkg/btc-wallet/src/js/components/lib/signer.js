import React from 'react';
import { Box, Button } from '@tlon/indigo-react';

const Signer = ({
  signReady,
  initPayment,
  choosingSignMethod,
  signMethod,
  setSignMethod,
}) => {
  return choosingSignMethod ? (
    <Box borderRadius="24px" backgroundColor="rgba(33, 157, 255, 0.2)">
      <Button
        border="none"
        backgroundColor="transparent"
        fontWeight="bold"
        cursor="pointer"
        color={signMethod === 'masterTicket' ? 'blue' : 'lightBlue'}
        height="48px"
        onClick={() => setSignMethod('masterTicket')}
      >
        Sign with Master Ticket
      </Button>
      <Button
        border="none"
        backgroundColor="transparent"
        fontWeight="bold"
        cursor="pointer"
        color={signMethod === 'bridge' ? 'blue' : 'lightBlue'}
        height="48px"
        onClick={() => setSignMethod('bridge')}
      >
        Sign with Bridge
      </Button>
    </Box>
  ) : (
    <Button
      primary
      fontSize={1}
      fontWeight="bold"
      borderRadius="24px"
      height="48px"
      onClick={initPayment}
      color={signReady ? 'white' : 'lighterGray'}
      backgroundColor={signReady ? 'blue' : 'veryLightGray'}
      disabled={!signReady}
      border="none"
      style={{ cursor: signReady ? 'pointer' : 'default' }}
    >
      {signMethod === 'bridge' ? 'Sign with Bridge' : 'Sign with Master Ticket'}
    </Button>
  );
};

export default Signer;
