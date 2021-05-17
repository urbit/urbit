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

import Sent from './sent.js'

import { satsToCurrency } from '../../lib/util.js';

window.bitcoin = bitcoin;
window.kg = kg;

export default class BridgeInvoice extends Component {
  constructor(props) {
    super(props);

    this.state = {
      externalPsbt: '',
      ready: false,
      error: false,
      sent: false,
    };

    this.checkExternalPsbt = this.checkExternalPsbt.bind(this);
    this.broadCastTx = this.broadCastTx.bind(this);
    this.sendBitcoin = this.sendBitcoin.bind(this);
  }

  broadCastTx(psbtHex) {
    let command = {
      'broadcast-tx': psbtHex
    }
    return this.props.api.btcWalletCommand(command)
  }

  componentDidMount() {
    window.open('https://bridge.urbit.org/?kind=btc&utx=' + this.props.psbt);
  }

  sendBitcoin(psbt) {

    try {
      const hex = bitcoin.Psbt.fromBase64(psbt).validateSignaturesOfAllInputs().toHex();
      this.broadCastTx(hex).then(res => this.setState({sent: true}));
    }

    catch(e) {
      this.setState({error: true});
    }
  }

  checkExternalPsbt(e){
    let externalPsbt = e.target.value;
    let ready = (externalPsbt.length > 0);
    let error = false;
    this.setState({externalPsbt, ready, error});
  }

  render() {
    const { stopSending, payee, denomination, satsAmount, psbt, currencyRates } = this.props;
    const { sent, error, externalPsbt } = this.state;

    let inputColor = 'black';
    let inputBg = 'white';
    let inputBorder = 'lightGray';

    if (error) {
      inputColor = 'red';
      inputBg = 'veryLightRed';
      inputBorder = 'red';
    }

    return (
      <>
        { sent ?
          <Sent
            payee={payee}
            stopSending={stopSending}
            denomination={denomination}
            currencyRates={currencyRates}
            satsAmount={satsAmount}
          /> :
          <Col
            height='400px'
            width='100%'
            backgroundColor='white'
            borderRadius='48px'
            mb={5}
            p={5}
          >
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
                  <Text fontSize='14px' fontWeight='500'>You are sending</Text>
                </Row>
                <Row
                  mt={2}
                >
                  <Text
                    color='green'
                    fontSize='14px'
                  >{satsToCurrency(satsAmount, denomination, currencyRates)}</Text>
                  <Text
                    ml={2}
                    fontSize='14px'
                    color='gray'
                  >{`${satsAmount} sats`}</Text>
                </Row>
                <Row
                  mt={2}
                >
                  <Text fontSize='14px'>To:</Text>
                  <Text
                    ml={2}
                    fontSize='14px'
                    style={{'display': 'block', 'overflow-wrap': 'anywhere'}}
                  >{payee}</Text>
                </Row>
              </Box>
            </Box>
            <Box mt={3}>
              <Text fontSize='14px' fontWeight='500'>
                Bridge signed transaction
              </Text>
            </Box>
            <Box mt={1} mb={2}>
              <Text gray fontSize='14px'>
                Copy the signed transaction from Bridge
              </Text>
            </Box>
            <Input
              value={this.state.externalPsbt}
              fontSize='14px'
              placeholder='cHNidP8BAHEBAAAAAXqmzdCZ4uv...'
              autoCapitalize='none'
              autoCorrect='off'
              color={inputColor}
              backgroundColor={inputBg}
              borderColor={inputBorder}
              style={{'line-height': '4'}}
              onChange={this.checkExternalPsbt}
            />
            {error &&
             <Row>
               <Text
                 fontSize='14px'
                 color='red'
                 mt={2}>
                 Invalid signed bitcoin transaction
               </Text>
             </Row>
            }
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
                onClick={() => this.sendBitcoin(externalPsbt)}
                disabled={!this.state.ready || error}
                style={{cursor: (this.state.ready && !error) ? "pointer" : "default"}}
              />
            </Row>
          </Col>
        }
      </>
    );
  }
}
