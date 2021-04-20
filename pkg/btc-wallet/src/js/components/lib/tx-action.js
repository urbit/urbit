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

import { Sigil } from './sigil.js'


export default class TxAction extends Component {
  constructor(props) {
    super(props);
  }


  render() {
    const leftIcon =
      (this.props.action === "sent") ? "ArrowSouth" :
      (this.props.action === "recv") ? "ArrowNorth" :
      (this.props.action === "fail") ? "X" :
      "NullIcon";

    const actionColor =
      (this.props.action === "sent") ? "sentBlue" :
      (this.props.action === "recv") ? "recvGreen" :
      (this.props.action === "fail") ? "gray" :
      "red";

    const actionText =
      (this.props.action === "sent" && !this.props.pending) ? "Sent BTC" :
      (this.props.action === "sent" &&  this.props.pending) ? "Sending BTC" :
      (this.props.action === "recv" && !this.props.pending) ? "Received BTC" :
      (this.props.action === "recv" &&  this.props.pending) ? "Receiving BTC" :
      (this.props.action === "fail") ? "Failed" :
      "error";

    const pending = (!this.props.pending) ? null :
      <LoadingSpinner
        background="midOrange"
        foreground="orange"
      />

    return (
      <Row alignItems="center">
        <Box backgroundColor={actionColor}
             width="24px"
             height="24px"
             textAlign="center"
             alignItems="center"
             borderRadius="2px"
             mr={2}
             p={1}
        >
          <Icon icon={leftIcon} color="white"/>
        </Box>
        <Text color={actionColor} fontSize="14px">{actionText}</Text>
        <Icon color={actionColor} icon="ArrowNorthEast" ml={1} mr={2}/>
        {pending}
      </Row>
    );
  }
}
