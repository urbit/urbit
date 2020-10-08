import React, { Component } from 'react';
import { Box, Text, Row, BaseInput } from '@tlon/indigo-react';

export class MetadataColor extends Component {
  constructor(props) {
    super(props);
    this.state = {
      color: props.initialValue
    };

    this.changeColor = this.changeColor.bind(this);
    this.submitColor = this.submitColor.bind(this);
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (prevProps.initialValue !== props.initialValue) {
      this.setState({ color: props.initialValue });
    }
  }

  changeColor(event) {
    this.setState({ color: event.target.value });
  }

  submitColor() {
    const { props, state } = this;

    let color = state.color;
    if (color.startsWith('#')) {
      color = state.color.substr(1);
    }
    const hexExp = /([0-9A-Fa-f]{6})/;
    const hexTest = hexExp.exec(color);
    if (!props.isDisabled && hexTest && (state.color !== props.initialValue)) {
      props.setValue(color);
    }
  }

  render() {
    const { props, state } = this;
    return (
      <Box
        width='100%'
        mb='3'
        opacity={(props.isDisabled) ? '0.3' : '1'}
      >
      <Text my='1' display='block' fontSize='1'>Change color</Text>
        <Text fontSize='0' gray display='block' mb='3'>Give this {props.resource} a color when viewing group channels</Text>
        <Row
          position='relative'
          maxWidth='10rem'
          width='100%'
        >
        <Box
          position='absolute'
          height='16px'
          width='16px'
          backgroundColor={state.color}
          style={{ top: 18, left: 11 }}
        />
        <BaseInput
          pl='5'
          fontSize='1'
          border='1px solid'
          borderColor='gray'
          backgroundColor='white'
          pt='3'
          pb='3'
          pr='3'
          display='block'
          width='100%'
          flex='auto'
          color='black'
          mr='3'
          value={state.color}
          disabled={props.isDisabled}
          onChange={this.changeColor}
          onBlur={this.submitColor}
        />
        </Row>
      </Box>
    );
  }
}
