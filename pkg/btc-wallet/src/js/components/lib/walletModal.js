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

import { patp2dec, isValidPatq } from 'urbit-ob';

const kg = require('urbit-key-generation');
const bitcoin = require('bitcoinjs-lib');
const bs58check = require('bs58check')
import { Buffer } from 'buffer';

export default class WalletModal extends Component {
  constructor(props) {
    super(props);

    this.state = {
      mode: 'masterTicket',
      masterTicket: '',
      confirmedMasterTicket: '',
      xpub: '',
      readyToSubmit: false,
      processingSubmission: false,
      confirmingMasterTicket: false,
      error: false,
    }
    this.checkTicket        = this.checkTicket.bind(this);
    this.checkXPub          = this.checkXPub.bind(this);
    this.submitMasterTicket = this.submitMasterTicket.bind(this);
    this.submitXPub         = this.submitXPub.bind(this);

  }

  checkTicket(e){
    // TODO: port over bridge ticket validation logic
    if (this.state.confirmingMasterTicket) {
      let confirmedMasterTicket = e.target.value;
      let readyToSubmit = isValidPatq(confirmedMasterTicket);
      this.setState({confirmedMasterTicket, readyToSubmit});
    } else {
      let masterTicket = e.target.value;
      let readyToSubmit = isValidPatq(masterTicket);
      this.setState({masterTicket, readyToSubmit});
    }
  }

  checkXPub(e){
    let xpub = e.target.value;
    let readyToSubmit = (xpub.length > 0);
    this.setState({xpub, readyToSubmit});
  }

  submitMasterTicket(ticket){
    this.setState({processingSubmission: true});
    kg.generateWallet({ ticket, ship: parseInt(patp2dec('~' + window.ship)) })
      .then(urbitWallet => {
        const { xpub } = this.props.network === 'testnet'
                       ? urbitWallet.bitcoinTestnet.keys :
                         urbitWallet.bitcoinMainnet.keys

        this.submitXPub(xpub);
      });

  }

  submitXPub(xpub){
    const command = {
      "add-wallet": {
        "xpub": xpub,
        "fprint": [4, 0],
        "scan-to": null,
        "max-gap": 8,
        "confs": 1
      }
    }
    api.btcWalletCommand(command);
    this.setState({processingSubmission: true});
  }

  render() {
    const buttonDisabled = (!this.state.readyToSubmit || this.state.processingSubmission );
    const inputDisabled = this.state.processingSubmission;
    const processingSpinner = (!this.state.processingSubmission) ? null :
      <LoadingSpinner/>

    if (this.state.mode === 'masterTicket'){
      return (
        <Box
          width="100%"
          height="100%"
          padding={3}
        >
          <Row>
            <Icon icon="Bitcoin" mr={2}/>
            <Text fontSize="14px" fontWeight="bold">
              Step 2 of 2: Import your extended public key
            </Text>
          </Row>
          <Box mt={3}>
            <Text fontSize="14px" fontWeight="regular" color="gray">
              To begin, you'll need to derive an XPub Bitcoin address using your
              master ticket. Learn More
            </Text>
          </Box>
          <Box
            display='flex'
            alignItems='center'
            mt={3}
            mb={2}>
            {this.state.confirmingMasterTicket &&
             <Icon
               icon='ArrowWest'
               cursor='pointer'
               onClick={() => this.setState({
                 confirmingMasterTicket: false,
                 masterTicket: '',
                 confirmedMasterTicket: '',
                 error: false
               })}/>}
            <Text fontSize="14px" fontWeight="500">
              {this.state.confirmingMasterTicket ? 'Confirm Master Key' : 'Master Key'}
            </Text>
          </Box>
          <Row alignItems="center">
            <StatelessTextInput
              mr={2}
              width="256px"
              value={this.state.confirmingMasterTicket ? this.state.confirmedMasterTicket : this.state.masterTicket}
              disabled={inputDisabled}
              fontSize="14px"
              type="password"
              name="masterTicket"
              obscure={value => value.replace(/[^~-]+/g, '••••••')}
              placeholder="••••••-••••••-••••••-••••••"
              autoCapitalize="none"
              autoCorrect="off"
              onChange={this.checkTicket}
            />
            {(!inputDisabled) ? null : <LoadingSpinner/>}
          </Row>
          {this.state.error &&
           <Row mt={2}>
             <Text
               fontSize='14px'
               color='red'>
               Master tickets do not match
             </Text>
           </Row>
          }
          <Box mt={3} mb={3}>
            <Text fontSize="14px" fontWeight="regular"
              color={(inputDisabled) ? "lighterGray" : "gray"}
              style={{cursor: (inputDisabled) ? "default" : "pointer"}}
              onClick={() => {
                if (inputDisabled) return;
                this.setState({mode: 'xpub', xpub: '', masterTicket: '', readyToSubmit: false})
              }}
            >
              Manually import your extended public key ->
            </Text>
          </Box>
          <Button
            primary
            disabled={buttonDisabled}
            children="Next Step"
            fontSize="14px"
            style={{cursor: buttonDisabled ? "default" : "pointer"}}
            onClick={() => {
              if (!this.state.confirmingMasterTicket) {
                this.setState({confirmingMasterTicket: true});
              } else {
                if (this.state.masterTicket === this.state.confirmedMasterTicket) {
                  this.setState({error: false});
                  this.submitMasterTicket(this.state.masterTicket);
                } else {
                  this.setState({error: true});
                }
              }
            }}
          />
        </Box>
      );
    } else if (this.state.mode === 'xpub') {
      return (
        <Box
          width="100%"
          height="100%"
          padding={3}
        >
          <Row>
            <Icon icon="Bitcoin" mr={2}/>
            <Text fontSize="14px" fontWeight="bold">
              Step 2 of 2: Import your extended public key
            </Text>
          </Row>
          <Box mt={3}>
            <Text fontSize="14px" fontWeight="regular" color="gray">
              Visit bridge.urbit.org to obtain your key
            </Text>
          </Box>
          <Box mt={3} mb={2}>
            <Text fontSize="14px" fontWeight="500">
              Extended Public Key (XPub)
            </Text>
          </Box>
          <StatelessTextInput
            value={this.state.xpub}
            disabled={inputDisabled}
            fontSize="14px"
            type="password"
            name="xpub"
            autoCapitalize="none"
            autoCorrect="off"
            onChange={this.checkXPub}
          />
          <Row mt={3}>
            <Button
              primary
              color="black"
              backgroundColor="veryLightGray"
              borderColor="veryLightGray"
              children="Cancel"
              fontSize="14px"
              mr={2}
              style={{cursor: "pointer"}}
              onClick={() => {this.setState({mode: 'masterTicket', masterTicket: '', xpub: '', readyToSubmit: false})}}
            />
            <Button
              primary
              disabled={buttonDisabled}
              children="Next Step"
              fontSize="14px"
              style={{cursor: this.state.ready ? "pointer" : "default"}}
              onClick={() => { this.submitXPub(this.state.xpub) }}
            />
          </Row>
        </Box>
      );
    }
  }
}
