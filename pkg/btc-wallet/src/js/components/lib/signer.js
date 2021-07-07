import React, { Component } from 'react';

import {
  Box,
  Button,
} from '@tlon/indigo-react';

export default function Signer(props) {
  const { signReady, initPayment, choosingSignMethod, signMethod, setSignMethod } = props;

  return (
    choosingSignMethod ?
    <Box
      borderRadius='24px'
      backgroundColor='rgba(33, 157, 255, 0.2)'
    >
      <Button
        border='none'
        backgroundColor='transparent'
        fontWeight='bold'
        cursor='pointer'
        color={(signMethod === 'masterTicket') ? 'blue' : 'lightBlue'}
        height='48px'
        onClick={() => setSignMethod('masterTicket')}
        children='Sign with Master Ticket' />
      <Button
        border='none'
        backgroundColor='transparent'
        fontWeight='bold'
        cursor='pointer'
        color={(signMethod === 'bridge') ? 'blue' : 'lightBlue'}
        height='48px'
        onClick={() => setSignMethod('bridge')}
        children='Sign with Bridge' />
    </Box>
    :
    <Button
      primary
      children={signMethod === 'bridge' ? 'Sign with Bridge' : 'Sign with Master Ticket'}
      fontSize={1}
      fontWeight='bold'
      borderRadius='24px'
      height='48px'
      onClick={initPayment}
      color={signReady ? 'white' : 'lighterGray'}
      backgroundColor={signReady ? 'blue' : 'veryLightGray'}
      disabled={!signReady}
      border='none'
      style={{cursor: signReady ? 'pointer' : 'default'}}
    />
  )
}
