import React from 'react';
import { Text } from '@tlon/indigo-react';

const errorToString = (error) => {
  if (error === 'cant-pay-ourselves') {
    return 'Cannot pay ourselves';
  }
  if (error === 'no-comets') {
    return 'Cannot pay comets';
  }
  if (error === 'no-dust') {
    return 'Cannot send dust';
  }
  if (error === 'tx-being-signed') {
    return 'Cannot pay when transaction is being signed';
  }
  if (error === 'insufficient-balance') {
    return 'Insufficient confirmed balance';
  }
  if (error === 'broadcast-fail') {
    return 'Transaction broadcast failed';
  }
  if (error === 'invalid-master-ticket') {
    return 'Invalid master ticket';
  }
  if (error === 'invalid-signed') {
    return 'Invalid signed bitcoin transaction';
  }
};

const Error = ({ error, ...rest }) => (
  <Text color="red" {...rest}>
    {errorToString(error)}
  </Text>
);

export default Error;
