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
import * as bip39 from 'bip39';

import Sent from './sent.js'
import { patp2dec, isValidPatq } from 'urbit-ob';

import { satsToCurrency } from '../../lib/util.js';
import Error from './error.js';

window.bitcoin = bitcoin;
window.kg = kg;
window.bip39 = bip39;

const BITCOIN_MAINNET_INFO = {
  messagePrefix: '\x18Bitcoin Signed Message:\n',
  bech32: 'bc',
  bip32: {
    public: 0x04b24746,
    private: 0x04b2430c,
  },
  pubKeyHash: 0x00,
  scriptHash: 0x05,
  wif: 0x80,
};

const BITCOIN_TESTNET_INFO = {
  messagePrefix: '\x18Bitcoin Signed Message:\n',
  bech32: 'tb',
  bip32: {
    public: 0x045f1cf6,
    private: 0x045f18bc,
  },
  pubKeyHash: 0x6f,
  scriptHash: 0xc4,
  wif: 0xef,
};

export default class Invoice extends Component {
  constructor(props) {
    super(props);

    this.state = {
      masterTicket: '',
      ready: false,
      error: this.props.state.error,
      sent: false,
      broadcasting: false,
    };

    this.checkTicket = this.checkTicket.bind(this);
    this.broadCastTx = this.broadCastTx.bind(this);
    this.sendBitcoin = this.sendBitcoin.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.broadcasting) {
      if (this.state.error !== '') {
        this.setState({broadcasting: false});
      }
    }
  }

  broadCastTx(psbtHex) {
    let command = {
      'broadcast-tx': psbtHex
    }
    return this.props.api.btcWalletCommand(command)
  }

  sendBitcoin(ticket, psbt) {
    const newPsbt = bitcoin.Psbt.fromBase64(psbt);
    this.setState({broadcasting: true});
    kg.generateWallet({ ticket, ship: parseInt(patp2dec('~' + window.ship)) })
      .then(urbitWallet => {
        const { xpub } = this.props.network === 'testnet'
          ? urbitWallet.bitcoinTestnet.keys
          : urbitWallet.bitcoinMainnet.keys;

        const { xprv: zprv } = urbitWallet.bitcoinMainnet.keys;
        const { xprv: vprv } = urbitWallet.bitcoinTestnet.keys;

        const isTestnet = (this.props.network === 'testnet');
        const derivationPrefix = isTestnet ? "m/84'/1'/0'/" : "m/84'/0'/0'/";

        const btcWallet = (isTestnet)
          ? bitcoin.bip32.fromBase58(vprv, BITCOIN_TESTNET_INFO)
          : bitcoin.bip32.fromBase58(zprv, BITCOIN_MAINNET_INFO);

        try {
          const hex = newPsbt.data.inputs
            .reduce((psbt, input, idx) => {
              //  removing already derived part, eg m/84'/0'/0'/0/0 becomes 0/0
              const path = input.bip32Derivation[0].path
                                .split(derivationPrefix)
                                .join('');
              const prv = btcWallet.derivePath(path).privateKey;
              return psbt.signInput(idx, bitcoin.ECPair.fromPrivateKey(prv));
            }, newPsbt)
            .finalizeAllInputs()
            .extractTransaction()
            .toHex();

          this.broadCastTx(hex);
        }
        catch(e) {
          this.setState({error: 'invalid-master-ticket', broadcasting: false});
        }
      });

  }


  checkTicket(e){
    // TODO: port over bridge ticket validation logic
    let masterTicket = e.target.value;
    let ready = isValidPatq(masterTicket);
    let error = (ready) ? '' : 'invalid-master-ticket';
    this.setState({masterTicket, ready, error});
  }

  render() {
    const broadcastSuccess = this.props.state.broadcastSuccess;
    const { stopSending, payee, denomination, satsAmount, psbt, currencyRates } = this.props;
    const { sent, error } = this.state;

    let inputColor = 'black';
    let inputBg = 'white';
    let inputBorder = 'lightGray';

    if (error !== '') {
      inputColor = 'red';
      inputBg = 'veryLightRed';
      inputBorder = 'red';
    }

    return (
      <>
        { broadcastSuccess ?
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
            <Box mt={3} mb={2}>
              <Text gray fontSize={1} fontWeight='600'>
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
              color={inputColor}
              backgroundColor={inputBg}
              borderColor={inputBorder}
              onChange={this.checkTicket}
            />
            {(error !== '') &&
             <Row>
               <Error
                 fontSize='14px'
                 color='red'
                 error={error}
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
                onClick={() => this.sendBitcoin(this.state.masterTicket, psbt)}
                disabled={!this.state.ready || error || this.state.broadcasting}
                style={{cursor: (this.state.ready && !error && !this.state.broadcasting) ? "pointer" : "default"}}
              />
              { (this.state.broadcasting) ? <LoadingSpinner mr={3}/> : null}
            </Row>
          </Col>
        }
      </>
    );
  }
}
