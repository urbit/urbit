import React from 'react';
import { Text } from '@tlon/indigo-react';

enum ErrorTypes {
  'cant-pay-ourselves' = 'Cannot pay ourselves',
  'no-comets' = 'Cannot pay comets',
  'no-dust' = 'Cannot send dust',
  'tx-being-signed' = 'Cannot pay when transaction is being signed',
  'insufficient-balance' = 'Insufficient confirmed balance',
  'broadcast-fail' = 'Transaction broadcast failed',
  'invalid-master-ticker' = 'Invalid master ticket',
  'invalid-signed' = 'Invalid signed bitcoin transaction',
}

const Error = ({
  error,
  fontSize,
  ...rest
}: {
  error: string;
  fontSize?: string;
}) => (
  <Text color="red" style={{ fontSize }} {...rest}>
    {
      (ErrorTypes as any)[
        Object.keys(ErrorTypes).filter((et) => et === error)[0]
      ]
    }
  </Text>
);

export default Error;
