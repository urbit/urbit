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
    if (!this.props.state.history || this.props.state.history.length <= 0) {
      return (
        <Box alignItems="center"
          display="flex"
          justifyContent="center"
          height="340px"
          width="100%"
          p={5}
          mb={5}
          borderRadius="32px"
          backgroundColor="white"
        >
          <Text color="gray" fontSize={2} fontWeight="bold">No Transactions Yet</Text>
        </Box>
      );
    } else {
      return (
        <Col
          width='100%'
          backgroundColor="white"
          borderRadius="32px"
          mb={5}
          p={5}
        >
          {
            this.props.state.history.map((tx, i) => {
              return(
                <Transaction
                  tx={tx}
                  key={i}
                  denom={this.props.state.denomination}
                  rates={this.props.state.currencyRates}
                />
              );
            })
          }
        </Col>
      );
    }
  }
}
