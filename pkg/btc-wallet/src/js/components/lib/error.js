import React, { Component } from 'react';
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
}

export default function Error(props) {
  const error = errorToString(props.error);

  return(
    <Text
      color='red'
      {...props}>
      {error}
    </Text>
  );
}
