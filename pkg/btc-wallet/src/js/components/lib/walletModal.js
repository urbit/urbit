import React, { Component } from 'react';
import {
  Box,
  Text,
  Button,
  StatelessTextInput,
  Icon,
  Row,
  Input,
} from '@tlon/indigo-react';

const kg = require('urbit-key-generation');
const bitcoin = require('bitcoinjs-lib');
const bs58check = require('bs58check')
import { Buffer } from 'buffer';

function bip44To84(network, xpub) {
  var prefix = (network === 'bitcoin') ? '04b24746' : '045f1cf6';
  console.log('xpub', xpub);
  var data = bs58check.decode(xpub);
  data = data.slice(4);
  data = Buffer.concat([Buffer.from(prefix, 'hex'), data]);
  return bs58check.encode(data);
}

const NETWORK = "testnet" // "bitcoin"
const DERIVATIONS = {
  bitcoin: "m/84'/0'/0'/0",
  testnet: "m/84'/1'/0'/0",
}
const BTC_DERIVATION_TYPE = 'bitcoin'
const BTC_DERIVATION_PATH = DERIVATIONS[NETWORK]

export default class WalletModal extends Component {
  constructor(props) {
    super(props);

    this.state = {
      mode: 'masterTicket',
      masterTicket: '',
      xpub: '',
      ready: false,
    }
    this.checkTicket        = this.checkTicket.bind(this);
    this.checkXPub          = this.checkXPub.bind(this);
    this.submitMasterTicket = this.submitMasterTicket.bind(this);
    this.submitXPub         = this.submitXPub.bind(this);
  }

  checkTicket(e){
    // TODO: port over bridge ticket validation logic
    let masterTicket = e.target.value;
    let ready = (masterTicket.length > 0);
    this.setState({masterTicket, ready});
  }

  checkXPub(e){
    let xpub = e.target.value;
    let ready = (xpub.length > 0);
    this.setState({xpub, ready});
  }

  submitMasterTicket(ticket){
    console.log("ticket", ticket);

    const node = kg.deriveNode(
      ticket,
      BTC_DERIVATION_TYPE,
      BTC_DERIVATION_PATH
    );

    const zpub = bip44To84(NETWORK,
      bitcoin.bip32.fromPublicKey(
        Buffer.from(node.keys.public, 'hex'),
        Buffer.from(node.keys.chain, 'hex'),
        bitcoin.networks[NETWORK]
      ).toBase58()
    );

    console.log('zpub', zpub);

    this.submitXPub(zpub);
  }

  submitXPub(xpub){
    const command = {
      "add-wallet": {
        "xpub": xpub,
        "fprint": [0, 0],
        "scan-to": null,
        "max-gap": 8,
        "confs": 1
      }
    }
    api.btcWalletCommand(command);
  }

  render() {
    if (this.state.mode === 'masterTicket'){
      return (
        <Box
          width="100%"
          height="100%"
          padding={3}
        >
          <Row>
            <Icon icon="NullIcon" mr={2}/>
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
          <Box mt={3} mb={2}>
            <Text fontSize="14px" fontWeight="500">
              Master Key
            </Text>
          </Box>
          <StatelessTextInput
            value={this.state.masterTicket}
            fontSize="14px"
            type="password"
            name="masterTicket"
            obscure={value => value.replace(/[^~-]+/g, '••••••')}
            placeholder="••••••-••••••-••••••-••••••"
            autoCapitalize="none"
            autoCorrect="off"
            onChange={this.checkTicket}
          />
          <Box mt={3} mb={3}>
            <Text fontSize="14px" fontWeight="regular" color="gray"
              style={{cursor: "pointer"}}
              onClick={() => { this.setState({mode: 'xpub', xpub: ''})}}
            >
              Manually import your extended public key ->
            </Text>
          </Box>
          <Button
            primary
            disabled={!this.state.ready}
            children="Next Step"
            fontSize="14px"
            style={{cursor: this.state.ready ? "pointer" : "default"}}
            onClick={() => {this.submitMasterTicket(this.state.masterTicket)}}
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
            <Icon icon="NullIcon" mr={2}/>
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
              onClick={() => {this.setState({mode: 'masterTicket', xpub: ''})}}
            />
            <Button
              primary
              disabled={!this.state.ready}
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
