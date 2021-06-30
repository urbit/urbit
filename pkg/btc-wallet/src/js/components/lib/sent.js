import React from 'react';
import { Icon, Row, Col, Center, Text } from '@tlon/indigo-react';
import { satsToCurrency } from '../../lib/util.js';
import { useSettings } from '../../hooks/useSettings';

const Sent = ({ payee, stopSending, satsAmount }) => {
  const { denomination, currencyRates } = useSettings();
  return (
    <Col
      height="400px"
      width="100%"
      backgroundColor="orange"
      borderRadius="48px"
      mb={5}
      p={5}
    >
      <Row flexDirection="row-reverse">
        <Icon color="white" icon="X" cursor="pointer" onClick={stopSending} />
      </Row>
      <Center>
        <Text
          style={{ display: 'block', overflowWrap: 'anywhere' }}
          color="white"
        >{`You sent BTC to ${payee}`}</Text>
      </Center>
      <Center flexDirection="column" flex="1 1 auto">
        <Text color="white" fontSize="40px">
          {satsToCurrency(satsAmount, denomination, currencyRates)}
        </Text>
        <Text color="white">{`${satsAmount} sats`}</Text>
      </Center>
    </Col>
  );
};

export default Sent;
