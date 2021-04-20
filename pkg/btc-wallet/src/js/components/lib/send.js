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

import Invoice from './invoice.js'

import * as ob from 'urbit-ob';

export default class Send extends Component {
  constructor(props) {
    super(props);

    this.state = {
      signing: false,
      denomAmount: '0.00',
      satsAmount: '0',
      payee: '',
      checkingPatp: false,
      payeeType: '',
      ready: false,
    };

    this.initPayment  = this.initPayment.bind(this);
    this.checkPayee  = this.checkPayee.bind(this);
  }

  checkPayee(e){
    let payee = e.target.value;
    let isPatp = ob.isValidPatp(payee);
    let isAddress = true; //TODO: actual validation

    if (isPatp) {
      let command = {'check-payee': payee}
      this.props.api.btcWalletCommand(command)
      this.setState({
        checkingPatp: true,
        payeeType: 'ship',
        payee,
      });
    } else if (isAddress) {
      this.setState({
        payee,
        ready: true,
        checkingPatp: false,
        payeeType: 'address',
      });
    } else {
      this.setState({
        payee,
        ready: false,
        checkingPatp: false,
        payeeType: '',
      });
    }
  }

  componentDidUpdate(prevProps, prevState) {
    if (!this.state.ready && this.state.checkingPatp) {
      if (this.props.shipWallets[this.state.payee.slice(1)]) {
        this.setState({ready: true, checkingPatp: false});
      }
    }
  }

  initPayment() {
    if (this.state.payeeType === 'ship') {
      let command = {
        'init-payment': {
          'payee': this.state.payee,
          'value': parseInt(this.state.satsAmount),
          'feyb': 1,
        }
      }
      this.props.api.btcWalletCommand(command).then(res => this.setState({signing: true}));
    } else if (this.state.payeeType === 'address') {
      let command = {
        'init-payment-external': {
          'address': this.state.payee,
          'value': parseInt(this.state.satsAmount),
          'feyb': 1,
        }
      }
      this.props.api.btcWalletCommand(command).then(res => this.setState({signing: true}));
    }
  }

  render() {

    const { api, value, conversion, stopSending, denomination } = this.props;
    const { denomAmount, satsAmount, signing, payee } = this.state;

    return (
      <>
        { signing ?
          <Invoice
            api={api}
            stopSending={stopSending}
            payee={payee}
            denomination={denomination}
            denomAmount={denomAmount}
            satsAmount={satsAmount}
          /> :
          <Col
            height='400px'
            width='100%'
            backgroundColor='white'
            borderRadius='32px'
            mb={5}
            p={5}
          >
            <Row
              justifyContent='space-between'
              alignItems='center'
            >
              <Text highlight color='blue' fontSize={1}>Send BTC</Text>
              <Text highlight color='blue' fontSize={1}>{value}</Text>
              <Icon
                icon='X'
                cursor='pointer'
                onClick={() => stopSending()}
              />
            </Row>
            <Row
              alignItems='center'
              mt={6}
              justifyContent='space-between'>
              <Text
                gray
                fontSize={1}
                width='40%'
                fontWeight='600'
              >
                To
              </Text>
              <Input
                width='100%'
                fontSize='14px'
                placeholder='~sampel-palnet or BTC address'
                value={payee}
                onChange={this.checkPayee}
              />
            </Row>
            <Row
              alignItems='center'
              mt={4}
              justifyContent='space-between'>
              <Text
                gray
                fontSize={1}
                width='40%'
                fontWeight='600'
              >Amount</Text>
              <Input
                fontSize='14px'
                width='100%'
                type='number'
                border='none'
                value={denomAmount}
                onChange={e => {
                  this.setState({
                    denomAmount: e.target.value,
                    satsAmount: Math.round(parseFloat(e.target.value) / conversion * 100000000)
                  });
                }}
              />
              <Text gray fontSize={1}>{denomination}</Text>
            </Row>
            <Row
              alignItems='center'
              mt={2}
              justifyContent='space-between'>
              {/* yes this is a hack */}
              <Box width='40%' />
              <Input
                fontSize='14px'
                width='100%'
                type='number'
                border='none'
                value={satsAmount}
                onChange={e => {
                  this.setState({
                    denomAmount: parseFloat(e.target.value) * (conversion / 100000000),
                    satsAmount: e.target.value
                  });
                }}
              />
              <Text gray fontSize={1}>sats</Text>
            </Row>
            <Row
              flexDirection='row-reverse'
              mt={8}
            >
              <Button
                primary
                children='Sign Transaction' mr={3}
                fontSize={1}
                borderRadius='24px'
                py='24px'
                px='24px'
                onClick={() =>{
                  this.initPayment()
                }}
                disabled={!this.state.ready}
                style={{cursor: this.state.ready ? "pointer" : "default"}}
              />
            </Row>
          </Col>
        }
      </>
    );
  }
}
