import React, { Component } from 'react';
import {
  Box,
  Icon,
  Row,
  Text,
  Button,
  Col,
  StatelessRadioButtonField as RadioButton,
  Label,
} from '@tlon/indigo-react';


export default class FeePicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      selected: 'mid'
    }

    this.select = this.select.bind(this);
  }

  select(which) {
    this.setState({selected: which});
    this.props.feeSelect(which);
  }

  render() {
    return (
      <Box
        position="absolute" p={4}
        border="1px solid green" zIndex={10}
        backgroundColor="white" borderRadius={3}
      >
        <Text fontSize={1} color="black" fontWeight="bold" mb={4}>
          Transaction Speed
        </Text>
        <Col mt={4}>
          <RadioButton
            name="feeRadio"
            selected={this.state.selected === 'low'}
            p="2"
            onChange={() => {
              this.select('low');
            }}
          >
            <Label fontSize="14px">Slow: {this.props.feeChoices.low[1]} sats/vbyte ~{this.props.feeChoices.low[0]}m</Label>
          </RadioButton>

          <RadioButton
            name="feeRadio"
            selected={this.state.selected === 'mid'}
            p="2"
            onChange={() => {
              this.select('mid');
            }}
          >
            <Label fontSize="14px">Normal: {this.props.feeChoices.mid[1]} sats/vbyte ~{this.props.feeChoices.mid[0]}m</Label>
          </RadioButton>

          <RadioButton
            name="feeRadio"
            selected={this.state.selected === 'high'}
            p="2"
            onChange={() => {
              this.select('high');
            }}
          >
            <Label fontSize="14px">Fast: {this.props.feeChoices.high[1]} sats/vbyte ~{this.props.feeChoices.high[0]}m</Label>
          </RadioButton>
        </Col>

      </Box>
    );
  }
}
