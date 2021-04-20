import React, { Component } from 'react';
import {
  Box,
  Icon,
  Row,
  Text,
  Button,
  Col,
} from '@tlon/indigo-react';

import Transaction from './transaction.js';


export default class Transactions extends Component {
  constructor(props) {
    super(props);
  }


  render() {
    const body = (this.props.state.history.length <= 0)
      ?  <Text color="gray" fontSize={2} fontWeight="bold">No Transactions Yet</Text>
      :  this.props.state.history.map((tx, i) => {
        return(
          <Transaction
            tx={tx}
            key={i}
            denom={this.props.state.denomination}
            rates={this.props.state.currencyRates}
          />
        );
      });

    return (
      <Col
        width='100%'
        backgroundColor="white"
        borderRadius="32px"
        mb={5}
        p={5}
      >
        {body}
      </Col>
    );
  }
}
