import React, { Component } from 'react';
import {
  Box,
  Icon,
  Row,
  Text,
} from '@tlon/indigo-react';

export default class Header extends Component {
  constructor(props) {
    super(props);
  }


  render() {
    return (
      <Row
        height={8}
        width='100%'
        justifyContent="space-between"
        alignItems="center"
        pt={5}
        pb={5}
      >
        <Row alignItems="center" justifyContent="center">
          <Box backgroundColor="orange"
            borderRadius={4} mr="12px"
            width={5}
            height={5}
            alignItems="center"
            justifyContent="center"
          >
            <Icon icon="Bitcoin" width={4} p={1} height={4} color="white"/>
          </Box>
          <Text fontSize={2} fontWeight="bold" color="orange">
            Bitcoin
          </Text>
        </Row>
        <Box backgroundColor="white"
          borderRadius={4}
          width={5}
          height={5}
          p={2}
        >
          <Icon icon="Adjust" color="orange" />
        </Box>
      </Row>
    );
  }
}
