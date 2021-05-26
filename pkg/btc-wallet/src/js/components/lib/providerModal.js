import React, { Component } from 'react';
import {
  Box,
  Text,
  Button,
  StatelessTextInput,
  Icon,
  Row,
  Input,
  LoadingSpinner,
} from '@tlon/indigo-react';

import { isValidPatp } from 'urbit-ob';

export default class ProviderModal extends Component {
  constructor(props) {
    super(props);

    this.state = {
      potentialProvider: null,
      checkingProvider: false,
      providerFailed: false,
      ready: false,
      provider: null,
      connecting: false,
    }

    this.checkProvider  = this.checkProvider.bind(this);
    this.submitProvider = this.submitProvider.bind(this);
  }

  checkProvider(e) {
    // TODO: loading states
    let provider = e.target.value;
    let ready = false;
    let checkingProvider = false;
    let potentialProvider = this.state.potentialProvider;

    if (isValidPatp(provider)) {
      let command = {
        "check-provider": provider
      }
      potentialProvider = provider;
      checkingProvider = true;
      this.props.api.btcWalletCommand(command);
      setTimeout(() => {
        this.setState({providerFailed: true, checkingProvider: false});
      }, 5000);
    }
    this.setState({provider, ready, checkingProvider, potentialProvider});
  }

  componentDidUpdate(prevProps, prevState){
    if (!this.state.ready){
      if (this.props.providerPerms[this.state.provider]) {
        this.setState({ready: true, checkingProvider: false, providerFailed: false});
      }
    }
  }

  submitProvider(e){
    if (this.state.ready){
      let command = {
        "set-provider": this.state.provider
      }
      this.props.api.btcWalletCommand(command);
      this.setState({connecting: true});
    }
  }

  render() {

    let workingNode = null;
    let workingColor = null;
    let workingBg = null;
    if (this.state.ready) {
      workingColor = "green";
      workingBg = "veryLightGreen"
      workingNode =
        <Box mt={3}>
          <Text fontSize="14px" color="green">
            {this.state.provider} is a working provider node
          </Text>
        </Box>
    } else if (this.state.providerFailed) {
      workingColor = "red";
      workingBg = "veryLightRed"
      workingNode =
        <Box mt={3}>
          <Text fontSize="14px" color="red">
            {this.state.potentialProvider} is not a working provider node
          </Text>
        </Box>
    }

    return (
      <Box
        width="100%"
        height="100%"
        padding={3}
      >
        <Row>
          <Icon icon="Bitcoin" mr={2}/>
          <Text fontSize="14px" fontWeight="bold">
            Step 1 of 2: Set up Bitcoin Provider Node
          </Text>
        </Row>
        <Box mt={3}>
          <Text fontSize="14px" fontWeight="regular" color="gray">
            In order to perform Bitcoin transaction in Landscape, you'll need to set a provider node. A provider node is an urbit which maintains a synced Bitcoin ledger. 
            <a fontSize="14px" target="_blank" href="https://urbit.org/bitcoin-wallet"> Learn More</a>
          </Text>
        </Box>
        <Box mt={3} mb={2}>
          <Text fontSize="14px" fontWeight="500">
            Provider Node
          </Text>
        </Box>
        <Row alignItems="center">
          <StatelessTextInput
            mr={2}
            width="256px"
            fontSize="14px"
            type="text"
            name="masterTicket"
            placeholder="e.g. ~zod"
            autoCapitalize="none"
            autoCorrect="off"
            mono
            backgroundColor={workingBg}
            color={workingColor}
            borderColor={workingColor}
            onChange={this.checkProvider}
          />
          {(this.state.checkingProvider) ? <LoadingSpinner/> : null}
        </Row>
        {workingNode}
        <Row alignItems="center" mt={3}>
          <Button
            mr={2}
            primary
            disabled={!this.state.ready}
            children="Set Peer Node"
            fontSize="14px"
            style={{cursor: this.state.ready ? "pointer" : "default"}}
            onClick={() => {this.submitProvider(this.state.provider)}}
          />
          {(this.state.connecting) ? <LoadingSpinner/> : null}
        </Row>
      </Box>
    );
  }
}
