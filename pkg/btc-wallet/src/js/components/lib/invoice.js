import React, { Component } from 'react';
import {
  Box,
  Icon,
  StatelessTextInput as Input,
  Row,
  Text,
  Button,
  Col,
} from '@tlon/indigo-react';

import * as bitcoin from 'bitcoinjs-lib';
import * as kg from 'urbit-key-generation';
import * as bip39 from 'bip39';

window.bitcoin = bitcoin;
window.kg = kg;
window.bip39 = bip39;

export default class Invoice extends Component {
  constructor(props) {
    super(props);

    this.state = {
      masterTicket: '',
      ready: false,
    };

    this.checkTicket = this.checkTicket.bind(this);
    this.broadCastTx = this.broadCastTx.bind(this);
    this.sendBitcoin = this.sendBitcoin.bind(this);
  }

  broadCastTx(psbtHex) {
    let command = {
      'broadcast-tx': psbtHex
    }
    this.props.api.btcWalletCommand(command);
  }

  sendBitcoin(ticket, psbt) {

    const mnemonic = kg.deriveNodeSeed(ticket, 'bitcoin');
    const seed = bip39.mnemonicToSeed(mnemonic);
    const hd = bitcoin.bip32.fromSeed(seed);

    const newPsbt = bitcoin.Psbt.fromBase64(psbt);

    const hex =
      newPsbt.data.inputs
             .reduce((psbt, input, idx) => {
               const path = input.bip32Derivation[0].path
               const prv = hd.derivePath(path).privateKey;
               return psbt.signInput(idx, bitcoin.ECPair.fromPrivateKey(prv));

             }, newPsbt)
             .finalizeAllInputs()
             .extractTransaction()
             .toHex();

    console.log({hex});
    this.broadCastTx(hex);
  }

  checkTicket(e){
    // TODO: port over bridge ticket validation logic
    let masterTicket = e.target.value;
    let ready = (masterTicket.length > 0);
    this.setState({masterTicket, ready});
  }

  render() {
    const { stopSending, payee, denomination, denomAmount, satsAmount } = this.props;
    return (
      <>
        <Row
          justifyContent='space-between'
          alignItems='center'
        >
          <Text bold fontSize={1}>Invoice</Text>
          <Icon
            icon='X'
            cursor='pointer'
            onClick={() => stopSending()}
          />
        </Row>
        <Box
          mt={4}
          backgroundColor='rgba(0, 159, 101, 0.05)'
          borderRadius='12px'
        >
          <Box
            padding={4}
          >
            <Row>
              <Text fontWeight='500'>You are sending</Text>
            </Row>
            <Row
              mt={2}
            >
              <Text
                color='green'
              >{`$${denomAmount} ${denomination}`}</Text>
              <Text
                ml={2}
                color='gray'
              >{`${satsAmount} sats`}</Text>
            </Row>
            <Row
              mt={2}
            >
              <Text>To:</Text>
              <Text
                ml={2}
              >{payee}</Text>
            </Row>
          </Box>
        </Box>
        <Box mt={3} mb={2}>
          <Text fontSize='14px' fontWeight='500'>
            Master Key
          </Text>
        </Box>
        <Input
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
        <Row
          flexDirection='row-reverse'
          mt={4}
        >
          <Button
            primary
            children='Send BTC'
            mr={3}
            fontSize={1}
            borderRadius='24px'
            py='24px'
            px='24px'
            onClick={() => this.sendBitcoin(this.state.masterTicket, store.state.psbt)}
            disabled={!this.state.ready}
            style={{cursor: this.state.ready ? "pointer" : "default"}}
          />
        </Row>

      </>
    );
  }
}
