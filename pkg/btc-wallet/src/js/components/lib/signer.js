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
        color={(signMethod === 'Sign Transaction') ? 'blue' : 'lightBlue'}
        py='24px'
        px='24px'
        onClick={() => setSignMethod('Sign Transaction')}
        children='Sign Transaction' />
      <Button
        border='none'
        backgroundColor='transparent'
        fontWeight='bold'
        cursor='pointer'
        color={(signMethod === 'Sign with Bridge') ? 'blue' : 'lightBlue'}
        py='24px'
        px='24px'
        onClick={() => setSignMethod('Sign with Bridge')}
        children='Sign with Bridge' />
    </Box>
    :
    <Button
      primary
      children={signMethod}
      fontSize={1}
      fontWeight='bold'
      borderRadius='24px'
      py='24px'
      px='24px'
      onClick={initPayment}
      color={signReady ? 'white' : 'lighterGray'}
      backgroundColor={signReady ? 'blue' : 'veryLightGray'}
      disabled={!signReady}
      border='none'
      style={{cursor: signReady ? 'pointer' : 'default'}}
    />
  )
}
