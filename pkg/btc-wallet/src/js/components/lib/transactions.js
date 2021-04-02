import React, { Component } from 'react';
import {
  Box,
  Icon,
  Row,
  Text,
  Button,
  Col,
} from '@tlon/indigo-react';


export default class Transactions extends Component {
  constructor(props) {
    super(props);
  }


  render() {
    return (
      <Col
        height="100px"
        width='100%'
        backgroundColor="white"
        borderRadius="32px"
        flexGrow="1"
        mb={5}
        p={5}
        justifyContent="center"
        alignItems="center"
      >
        <Text color="gray" fontSize={2} fontWeight="bold">No Transactions Yet</Text>
      </Col>
    );
  }
}
