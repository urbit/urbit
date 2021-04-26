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

import { satsToCurrency } from '../../lib/util.js';

export default function Sent(props) {
  const { payee, denomination, satsAmount, stopSending, currencyRates } = props;
  return (
    <Col
      height='400px'
      width='100%'
      backgroundColor='orange'
      borderRadius='48px'
      mb={5}
      p={5}
    >
      <Row
        flexDirection='row-reverse'
      >
        <Icon
          color='white'
          icon='X'
          cursor='pointer'
          onClick={stopSending}
        />
      </Row>
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
          {satsToCurrency(satsAmount, denomination, currencyRates)}
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
