import React, { Component } from 'react';
import {
  Box,
  Icon,
  Row,
  Text,
  Button,
  Col,
} from '@tlon/indigo-react';

import { store } from '../../store'



export default class Warning extends Component {
  constructor(props) {
    super(props);

    this.understand = this.understand.bind(this);
  }

  understand(){
    store.handleEvent({ data: { bucket: { warning: false}}});
    let removeWarning = {
      "put-entry": {
        value: false,
        "entry-key": "warning",
        "bucket-key": "btc-wallet",
      }
    }
    this.props.api.settingsEvent(removeWarning);
  }

  render() {
    return (
      <Box
        backgroundColor="red"
        color="white"
        borderRadius="32px"
        justifyContent="space-between"
        width='100%'
        p={5}
        mb={5}
      >
        <Col>
          <Text color="white" fontWeight='bold' fontSize={1}>
            Warning!
          </Text>
          <br/>
          <Text color="white" fontWeight='bold' fontSize={1}>
            Be safe while using this wallet, and be sure to store responsible amounts
            of BTC.
          </Text>
          <Text color="white" fontWeight='bold' fontSize={1}>
            Always ensure that the checksum of the wallet matches that of the wallet's repo.
          </Text>
          <br/>
          <Text  color="white" fontWeight='bold' fontSize={1} style={{textDecoration: 'underline'}}>
            Learn more on urbit.org
          </Text>
        </Col>
        <Button children="I Understand"
          backgroundColor="white"
          fontSize={1}
          mt={5}
          color="red"
          fontWeight="bold"
          borderRadius="24px"
          p="24px"
          borderColor="none"
          onClick={this.understand}
        />
      </Box>
    );
  }
}
