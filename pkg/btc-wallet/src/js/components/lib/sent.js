import React, { Component } from 'react';
import {
  Box,
  Icon,
  StatelessTextInput as Input,
  Row,
  Center,
  Text,
  Button,
  Col,
} from '@tlon/indigo-react';

export default function Sent(props) {
  const { payee, denomination, denomAmount, satsAmount } = props;
  return (
    <Col
      height='400px'
      width='100%'
      backgroundColor='orange'
      borderRadius='32px'
      mb={5}
      p={5}
    >
      <Center>
        <Text color='white'>{`You sent BTC to ${payee}`}</Text>
      </Center>
      <Center
        flexDirection='column'
        flex='1 1 auto'
      >
        <Text
          color='white'
          fontSize='52px'
        >
          {`$${denomAmount}`}
        </Text>
        <Text
          color='white'
        >
          {`${satsAmount} sats`}
        </Text>
      </Center>
    </Col>
  );
}
