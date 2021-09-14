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

import { satsToCurrency } from '../../lib/util.js'
import { store } from '../../store';

export default class CurrencyPicker extends Component {
  constructor(props) {
    super(props);
    this.switchCurrency = this.switchCurrency.bind(this);
  }

  switchCurrency(){
    let newCurrency;
    if (this.props.denomination === 'BTC') {
      if (this.props.currencies['USD']) {
        newCurrency = "USD";
      }
    } else if (this.props.denomination === 'USD') {
      newCurrency = "BTC";
    }
    let setCurrency = {
      "put-entry": {
        desk: window.desk,
        value: newCurrency,
        "entry-key": "currency",
        "bucket-key": "btc-wallet",
      }
    }
    this.props.api.settingsEvent(setCurrency);
  }


  render() {
    return (
      <Row style={{cursor: "pointer"}} onClick={this.switchCurrency}>
        <Icon icon="ChevronDouble" color="orange" pt="2px" pr={1} />
        <Text color="orange" fontSize={1}>{this.props.denomination}</Text>
      </Row>
    );
  }
}
