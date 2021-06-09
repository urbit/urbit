import React, { Component } from 'react';
import {
  Box,
  Icon,
  Row,
  Text,
  Button,
  Col,
} from '@tlon/indigo-react';

export default class Settings extends Component {
  constructor(props) {
    super(props);
    this.changeProvider = this.changeProvider.bind(this);
    this.replaceWallet  = this.replaceWallet.bind(this);
  }

  changeProvider(){
    this.props.api.btcWalletCommand({'set-provider': null});
  }

  replaceWallet(){
    this.props.api.btcWalletCommand({
      'delete-wallet': this.props.state.wallet,
    });
  }

  render() {
    let connColor = "red";
    let connBackground = "veryLightRed";
    let conn = 'Offline'
    let host = '';
    if (this.props.state.provider){
      if (this.props.state.provider.connected) conn = 'Connected';
      if (this.props.state.provider.host) host = this.props.state.provider.host;
      if (this.props.state.provider.connected && this.props.state.provider.host) {
        connColor = "orange";
        connBackground = "lightOrange";
      }
    }

    return (
      <Col
        display="flex"
        width="100%"
        p={5}
        mb={5}
        borderRadius="48px"
        backgroundColor="white"
      >
        <Row mb="12px">
          <Text fontSize={1} fontWeight="bold" color="black">
            XPub Derivation
          </Text>
        </Row>
        <Row borderRadius="12px"
          backgroundColor="veryLightGray"
          py={5}
          px="36px"
          mb="12px"
          alignItems="center"
          justifyContent="space-between"
        >
          <Text mono
            fontSize={1}
            style={{wordBreak: "break-all"}}
            color="gray"
          >
            {this.props.state.wallet}
          </Text>
        </Row>
        <Row width="100%" mb={5}>
          <Button children="Replace Wallet"
            width="100%"
            fontSize={1}
            fontWeight="bold"
            backgroundColor="gray"
            color="white"
            borderColor="none"
            borderRadius="12px"
            p={4}
            onClick={this.replaceWallet}
          />
        </Row>
        <Row mb="12px">
          <Text fontSize={1} fontWeight="bold" color="black">
            BTC Node Provider
          </Text>
        </Row>
        <Col mb="12px"
          py={5}
          px="36px"
          borderRadius="12px"
          backgroundColor={connBackground}
          alignItems="center"
          justifyContent="space-between"
        >
          <Text fontSize={1} color={connColor} mono>
            ~{host}
          </Text>
          <Text fontSize={0} color={connColor}>
            {conn}
          </Text>
        </Col>
        <Row width="100%">
          <Button children="Change Provider"
            width="100%"
            fontSize={1}
            fontWeight="bold"
            backgroundColor="orange"
            color="white"
            borderColor="none"
            borderRadius="12px"
            p={4}
            onClick={this.changeProvider}
          />
        </Row>
      </Col>
    );
  }
}
