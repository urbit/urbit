import React, { Component } from 'react';
import {
  Box,
  Icon,
  Row,
  Text,
  Button,
  Col,
  LoadingSpinner,
} from '@tlon/indigo-react';
import _ from 'lodash';

import { Sigil } from './sigil.js'
import TxAction from './tx-action.js'
import TxCounterparty from './tx-counterparty.js'

export default class Transaction extends Component {
  constructor(props) {
    super(props);
  }


  render() {
    const pending = (!this.props.tx.recvd);
    console.log("transaction", this.props.tx.recvd);

    const weSent = _.find(this.props.tx.inputs, (input) => {
      return (input.ship === window.ship);
    });

    let action = (weSent) ? "sent" : "recv";

    let counterShip;
    let counterAddress;
    let value;
    let sign;

    if (action === "sent") {
      let counter = _.find(this.props.tx.outputs, (output) => {
        return (output.ship !== window.ship);
      });
      counterShip = counter.ship;
      counterAddress = counter.val.address;
      value = counter.val.value;
      sign = '-'
    }
    else if (action === "recv") {
      let incoming = _.find(this.props.tx.outputs, (output) => {
        return (output.ship === window.ship);
      });
      value = incoming.val.value;

      let counter = _.find(this.props.tx.inputs, (input) => {
        return (input.ship !== window.ship);
      });
      counterShip = counter.ship;
      counterAddress = counter.val.address;
      sign = '';
    }

    const failure = Boolean(this.props.tx.failure);
    if (failure) action = "fail";


    return (
      <Col
        width='100%'
        backgroundColor="white"
        justifyContent="space-between"
        mb="16px"
      >
        <Row justifyContent="space-between" alignItems="center">
          <TxAction action={action} pending={pending}/>
          <Text fontSize="14px" alignItems="center" color="gray">
            {sign}{value} sats
          </Text>
        </Row>
        <Box ml="11px" borderLeft="2px solid black" height="4px">
        </Box>
        <Row justifyContent="space-between" alignItems="center">
          <TxCounterparty address={counterAddress} ship={counterShip}/>
          <Text fontSize="14px">{sign}$5</Text>
        </Row>
      </Col>
    );
  }
}
