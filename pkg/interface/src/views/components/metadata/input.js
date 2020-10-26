import React, { Component } from 'react';
import { Box, Text, Row, BaseInput } from '@tlon/indigo-react';

export class MetadataInput extends Component {

  constructor(props) {
    super(props);

    this.state = {
      value: props.initialValue
    };
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (prevProps.initialValue !== props.initialValue) {
      this.setState({ value: props.initialValue });
    }
  }

  render() {
    const {
      title,
      description,
      isDisabled,
      setValue
    } = this.props;

    return (
      <Box
        width='100%'
        mb='3'
        opacity={(isDisabled) ? '0.3' : '1'}
      >
        <Text display='block' fontSize='1' mb='1'>{title}</Text>
        <Text display='block' mb='4' fontSize='0' gray>{description}</Text>
        <Row
          width='100%'
          position='relative'
          maxWidth='29rem'
        >
          <BaseInput
            fontSize='1'
            border='1px solid'
            borderColor='gray'
            backgroundColor='white'
            p='3'
            display='block'
            color='black'
            width='100'
            flex='auto'
            mr='3'
            type="text"
            value={this.state.value}
            disabled={isDisabled}
            onChange={(e) => {
              this.setState({ value: e.target.value });
            }}
            onBlur={() => {
              if (!isDisabled) {
                setValue(this.state.value || '');
              }
            }}
          />
        </Row>
      </Box>
    );
  }
}
