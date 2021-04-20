import React, { Component } from 'react';
import {
  Box,
  Icon,
  Row,
  Text,
  Button,
  Col,
} from '@tlon/indigo-react';

import { Sigil } from './sigil.js'
import TxAction from './tx-action.js'

export default class TxCounterparty extends Component {
  constructor(props) {
    super(props);
  }


  render() {
    const icon = (this.props.ship)
      ?  <Sigil
          ship={this.props.ship}
          size={24}
          color="black"
          classes={''}
          icon
          padding={5}
        />
      : <Box backgroundColor="lighterGray"
          width="24px"
          height="24px"
          textAlign="center"
          alignItems="center"
          borderRadius="2px"
          p={1}
        >
          <Icon icon="NullIcon" color="gray"/>
        </Box>
    const addressText = this.props.address.slice(0, 6) + '...' +
      this.props.address.slice(-6);
    const text = (this.props.ship) ?
      `~${this.props.ship}` : addressText;

    return (
      <Row alignItems="center">
        {icon}
        <Text ml={2} mono fontSize="14px" color="gray">{text}</Text>
      </Row>
    );
  }
}
