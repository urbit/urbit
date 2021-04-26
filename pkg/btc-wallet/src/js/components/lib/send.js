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

import Invoice from './invoice.js'

import { validate } from 'bitcoin-address-validation';

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
      validPayee: false,
      focusPayee: true,
      focusCurrency: false,
      focusSats: false,
      submitting: false,
    };

    this.initPayment  = this.initPayment.bind(this);
    this.checkPayee  = this.checkPayee.bind(this);
  }

  checkPayee(e){
    let payee = e.target.value;
    let isPatp = ob.isValidPatp(payee);
    let isAddress = validate(payee);

    if (isPatp) {
      let command = {'check-payee': payee}
      this.props.api.btcWalletCommand(command)
      setTimeout(() => {
        this.setState({checkingPatp: false});
      }, 5000);
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
        validPayee: true,
      });
    } else {
      this.setState({
        payee,
        ready: false,
        checkingPatp: false,
        payeeType: '',
        validPayee: false,
      });
    }
  }

  componentDidUpdate(prevProps, prevState) {
    if (!this.state.ready && this.state.checkingPatp) {
      if (this.props.shipWallets[this.state.payee.slice(1)]) {
        this.setState({ready: true, checkingPatp: false, validPayee: true});
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
    let payeeColor = "black";
    let payeeBg = "white";
    let payeeBorder = "lightGray";
    if (this.state.focusPayee && this.state.validPayee) {
      payeeColor = "green";
      payeeBorder = "green";
      payeeBg = "veryLightGreen";
    } else if (!this.state.focusPayee && this.state.validPayee){
      payeeColor="blue";
      payeeBorder = "white";
      payeeBg = "white";
    } else if (!this.state.focusPayee && !this.state.validPayee) {
      payeeColor="red";
      payeeBorder = "red";
      payeeBg="veryLightRed";
    } else if (this.state.focusPayee &&
              !this.state.validPayee &&
              !this.state.checkingPatp &&
              this.state.payeeType === 'ship'){
      payeeColor="red";
      payeeBorder = "red";
      payeeBg="veryLightRed";
    }


    const { api, value, conversion, stopSending, denomination, psbt, currencyRates } = this.props;
    const { denomAmount, satsAmount, signing, payee } = this.state;

    const signReady = (this.state.ready && (parseInt(this.state.satsAmount) > 0)) && !signing;

    return (
      <>
        { (signing && psbt) ?
          <Invoice
            api={api}
            psbt={psbt}
            currencyRates={currencyRates}
            stopSending={stopSending}
            payee={payee}
            denomination={denomination}
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
              <Row justifyContent="space-between" width='calc(40% - 30px)' alignItems="center">
                <Text gray fontSize={1} fontWeight='600'>To</Text>
                {this.state.checkingPatp ?
                  <LoadingSpinner background="midOrange" foreground="orange"/> : null
                }
              </Row>
              <Input
                autoFocus
                onFocus={() => {this.setState({focusPayee: true})}}
                onBlur={() => {this.setState({focusPayee: false})}}
                color={payeeColor}
                backgroundColor={payeeBg}
                borderColor={payeeBorder}
                ml={2}
                flexGrow="1"
                fontSize='14px'
                placeholder='~sampel-palnet or BTC address'
                value={payee}
                fontFamily="mono"
                disabled={signing}
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
                fontWeight='600'
                width="40%"
              >Amount</Text>
              <Input
                onFocus={() => {this.setState({focusCurrency: true})}}
                onBlur={() => {this.setState({focusCurrency: false})}}
                fontSize='14px'
                width='100%'
                type='number'
                borderColor={this.state.focusCurrency ? "lightGray" : "none"}
                disabled={signing}
                value={denomAmount}
                onChange={e => {
                  this.setState({
                    denomAmount: e.target.value,
                    satsAmount: Math.round(parseFloat(e.target.value) / conversion * 100000000)
                  });
                }}
              />
              <Text color="lighterGray" fontSize={1} ml={3}>{denomination}</Text>
            </Row>
            <Row
              alignItems='center'
              mt={2}
              justifyContent='space-between'>
              {/* yes this is a hack */}
              <Box width='40%'/>
              <Input
                onFocus={() => {this.setState({focusSats: true})}}
                onBlur={() => {this.setState({focusSats: false})}}
                fontSize='14px'
                width='100%'
                type='number'
                borderColor={this.state.focusSats ? "lightGray" : "none"}
                disabled={signing}
                value={satsAmount}
                onChange={e => {
                  this.setState({
                    denomAmount: parseFloat(e.target.value) * (conversion / 100000000),
                    satsAmount: e.target.value
                  });
                }}
              />
              <Text color="lightGray" fontSize={1} ml={3}>sats</Text>
            </Row>
            <Row
              flexDirection='row-reverse'
              alignItems="center"
              mt={7}
            >
              <Button
                primary
                children='Sign Transaction' 
                fontSize={1}
                fontWeight='bold'
                borderRadius='24px'
                mt={4}
                py='24px'
                px='24px'
                onClick={() =>{
                  this.initPayment()
                }}
                color={signReady ? "white" : "lighterGray"}
                backgroundColor={signReady ? "blue" : "veryLightGray"}
                disabled={!signReady}
                border="none"
                style={{cursor: signReady ? "pointer" : "default"}}
              />
              { (!signing) ? null :
                <LoadingSpinner mr={2} background="midOrange" foreground="orange"/>
              }
            </Row>
          </Col>
        }
      </>
    );
  }
}
