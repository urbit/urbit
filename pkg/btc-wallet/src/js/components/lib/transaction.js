import React from 'react';
import { Box, Row, Text, Col } from '@tlon/indigo-react';
import _ from 'lodash';
import TxAction from './tx-action.js';
import TxCounterparty from './tx-counterparty.js';
import { satsToCurrency } from '../../lib/util.js';
import { useSettings } from '../../hooks/useSettings.js';

const Transaction = ({ tx }) => {
  const { denomination, currencyRates } = useSettings();
  const pending = !tx.recvd;

  let weSent = _.find(tx.inputs, (input) => {
    return input.ship === window.ship;
  });
  let weRecv = tx.outputs.every((output) => {
    return output.ship === window.ship;
  });

  let action = weRecv ? 'recv' : weSent ? 'sent' : 'recv';

  let counterShip = null;
  let counterAddress = null;
  let value;
  let sign;

  if (action === 'sent') {
    let counter = _.find(tx.outputs, (output) => {
      return output.ship !== window.ship;
    });
    counterShip = _.get(counter, 'ship', null);
    counterAddress = _.get(counter, 'val.address', null);
    value = _.get(counter, 'val.value', null);
    sign = '-';
  } else if (action === 'recv') {
    value = _.reduce(
      tx.outputs,
      (sum, output) => {
        if (output.ship === window.ship) {
          return sum + output.val.value;
        } else {
          return sum;
        }
      },
      0
    );

    if (weSent && weRecv) {
      counterAddress = _.get(
        _.find(tx.inputs, (input) => {
          return input.ship === window.ship;
        }),
        'val.address',
        null
      );
    } else {
      let counter = _.find(tx.inputs, (input) => {
        return input.ship !== window.ship;
      });
      counterShip = _.get(counter, 'ship', null);
      counterAddress = _.get(counter, 'val.address', null);
    }
    sign = '';
  }

  let currencyValue = sign + satsToCurrency(value, denomination, currencyRates);

  const failure = Boolean(tx.failure);
  if (failure) action = 'fail';

  const txid = tx.txid.dat.slice(2).replaceAll('.', '');

  return (
    <Col
      width="100%"
      backgroundColor="white"
      justifyContent="space-between"
      mb="16px"
    >
      <Row justifyContent="space-between" alignItems="center">
        <TxAction action={action} pending={pending} txid={txid} />
        <Text fontSize="14px" alignItems="center" color="gray">
          {sign}
          {value} sats
        </Text>
      </Row>
      <Box ml="11px" borderLeft="2px solid black" height="4px"></Box>
      <Row justifyContent="space-between" alignItems="center">
        <TxCounterparty address={counterAddress} ship={counterShip} />
        <Text fontSize="14px">{currencyValue}</Text>
      </Row>
    </Col>
  );
};

export default Transaction;
