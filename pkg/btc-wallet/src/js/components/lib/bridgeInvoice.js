import React, { Component } from 'react';
import {
  Box,
  Icon,
  StatelessTextInput as Input,
  Row,
  Text,
  Button,
  Col,
  LoadingSpinner,
} from '@tlon/indigo-react';

import * as bitcoin from 'bitcoinjs-lib';
import * as kg from 'urbit-key-generation';

import Sent from './sent.js'
import Error from './error.js'

import { satsToCurrency } from '../../lib/util.js';

export default class BridgeInvoice extends Component {
  constructor(props) {
    super(props);

    this.state = {
      txHex: '',
      ready: false,
      error: this.props.state.error,
      broadcasting: false,
    };

    this.checkTxHex = this.checkTxHex.bind(this);
    this.broadCastTx = this.broadCastTx.bind(this);
    this.sendBitcoin = this.sendBitcoin.bind(this);
  }

  broadCastTx(hex) {
    let command = {
      'broadcast-tx': hex
    }
    return this.props.api.btcWalletCommand(command)
  }

  componentDidMount() {
    window.open('https://bridge.urbit.org/?kind=btc&utx=' + this.props.psbt);
  }

  componentDidUpdate(prevProps){
    if (this.state.broadcasting) {
      if (this.state.error !== '') {
        this.setState({broadcasting: false});
      }
    }

    if (prevProps.state.error !== this.props.state.error) {
      this.setState({error: this.props.state.error});
    }
  }

  sendBitcoin(hex) {
    try {
      bitcoin.Transaction.fromHex(hex)
      this.broadCastTx(hex)
      this.setState({broadcasting: true});
    }

    catch(e) {
      this.setState({error: 'invalid-signed', broadcasting: false});
    }
  }

  checkTxHex(e){
    let txHex = e.target.value;
    let ready = (txHex.length > 0);
    let error = '';
    this.setState({txHex, ready, error});
  }

  render() {
    const { stopSending, payee, denomination, satsAmount, psbt, currencyRates } = this.props;
    const { error, txHex } = this.state;

    let inputColor = 'black';
    let inputBg = 'white';
    let inputBorder = 'lightGray';

    if (error !== '') {
      inputColor = 'red';
      inputBg = 'veryLightRed';
      inputBorder = 'red';
    }

    console.log('bridge invoice', error);

    return (
      <>
        { this.props.state.broadcastSuccess ?
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
              value={this.state.txHex}
              fontSize='14px'
              placeholder='010000000001019e478cc370323ac539097...'
              autoCapitalize='none'
              autoCorrect='off'
              color={inputColor}
              backgroundColor={inputBg}
              borderColor={inputBorder}
              style={{'line-height': '4'}}
              onChange={this.checkTxHex}
            />
            { (error !== '') &&
             <Row>
               <Error
                 error={error}
                 fontSize='14px'
                 mt={2}/>
             </Row>
            }
            <Row
              flexDirection='row-reverse'
              mt={4}
              alignItems="center"
            >
              <Button
                primary
                children='Send BTC'
                mr={3}
                fontSize={1}
                borderRadius='24px'
                py='24px'
                px='24px'
                onClick={() => this.sendBitcoin(txHex)}
                disabled={!this.state.ready || error}
                style={{cursor: (this.state.ready && !error) ? "pointer" : "default"}}
              />
              {this.state.broadcasting ? <LoadingSpinner mr={3}/> : null}
            </Row>
          </Col>
        }
      </>
    );
  }
}
