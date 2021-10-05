import React from 'react';
import { Box, Button, Icon, Row } from '@tlon/indigo-react';
import { signMethods } from './Send';

const signMethodLabels = {
  bridge: 'Sign with Bridge',
  masterTicket: 'Sign with Master Ticket',
  external: 'Sign Externally (PSBT)',
};

type Props = {
  signReady: boolean;
  initPayment: () => void;
  choosingSignMethod: boolean;
  signMethod: signMethods;
  setSignMethod: (arg: signMethods) => void;
};

const Signer: React.FC<Props> = ({
  signReady,
  initPayment,
  choosingSignMethod,
  signMethod,
  setSignMethod,
}) => {
  return choosingSignMethod ? (
    <Box borderRadius="24px" backgroundColor="rgba(33, 157, 255, 0.2)">
      {Object.keys(signMethods).map((method) => (
        <Row key={method} flexDirection="row" alignItems="center">
          <Button
            border="none"
            backgroundColor="transparent"
            fontWeight="bold"
            cursor="pointer"
            color={
              signMethod === (signMethods as any)[method] ? 'blue' : 'lightBlue'
            }
            height="48px"
            onClick={() => setSignMethod((signMethods as any)[method])}
          >
            {(signMethodLabels as any)[method]}
          </Button>
          {signMethod === (signMethods as any)[method] && (
            <Button
              borderRadius="24px"
              width="24px"
              height="24px"
              backgroundColor="blue"
              border="none"
              padding="0px"
              mr="12px"
            >
              <Icon width="12px" icon="Checkmark" color="white" />
            </Button>
          )}
        </Row>
      ))}
    </Box>
  ) : (
    <Button
      primary
      fontSize={1}
      fontWeight="bold"
      borderRadius="24px"
      mr={2}
      height="48px"
      onClick={initPayment}
      color={signReady ? 'white' : 'lighterGray'}
      backgroundColor={signReady ? 'blue' : 'veryLightGray'}
      disabled={!signReady}
      border="none"
      style={{ cursor: signReady ? 'pointer' : 'default' }}
    >
      {(signMethodLabels as any)[signMethod]}
    </Button>
  );
};

export default Signer;
