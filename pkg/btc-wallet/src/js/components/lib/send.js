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
  StatelessRadioButtonField as RadioButton,
} from '@tlon/indigo-react';

import Invoice from './invoice.js'
import BridgeInvoice from './bridgeInvoice.js'
import FeePicker from './feePicker.js'
import Error from './error.js'
import Signer from './signer.js'

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
      focusNote: false,
      submitting: false,
      feeChoices: {
        low: [10, 1],
        mid: [10, 1],
        high: [10, 1],
      },
      feeValue: "mid",
      showModal: false,
      note: '',
      choosingSignMethod: false,
      signMethod: 'Sign Transaction',
    };

    this.initPayment  = this.initPayment.bind(this);
    this.checkPayee  = this.checkPayee.bind(this);
    this.feeSelect = this.feeSelect.bind(this);
    this.feeDismiss = this.feeDismiss.bind(this);
    this.toggleSignMethod = this.toggleSignMethod.bind(this);
    this.setSignMethod = this.setSignMethod.bind(this);
  }

  feeDismiss() {
    this.setState({showModal: false});
  }

  feeSelect(which) {
    this.setState({feeValue: which});
  }

  componentDidMount(){
    if (this.props.network === 'bitcoin'){
      let url = "https://bitcoiner.live/api/fees/estimates/latest";
      fetch(url).then(res => res.json()).then(n => {
        let estimates = Object.keys(n.estimates);
        let mid = Math.floor(estimates.length/2)
        let high = estimates.length - 1;
        this.setState({
          feeChoices: {
            low: [30, n.estimates[30]["sat_per_vbyte"]],
            mid: [180, n.estimates[180]["sat_per_vbyte"]],
            high: [360, n.estimates[360]["sat_per_vbyte"]],
          }
        });
      })
    }
  }

  setSignMethod(signMethod) {
    this.setState({signMethod});
  }

  checkPayee(e){
    store.handleEvent({data: {error: ''}});

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
    if ((prevProps.error !== this.props.error) &&
       (this.props.error !== '') && (this.props.error !== 'broadcast-fail')) {
      this.setState({signing: false});
    }

    if (!this.state.ready && this.state.checkingPatp) {
      if (this.props.shipWallets[this.state.payee.slice(1)]) {
        this.setState({ready: true, checkingPatp: false, validPayee: true});
      }
    }
  }

  toggleSignMethod(toggle) {
    this.setState({choosingSignMethod: !toggle});
  }

  initPayment() {
    if (this.state.payeeType === 'ship') {
      let command = {
        'init-payment': {
          'payee': this.state.payee,
          'value': parseInt(this.state.satsAmount),
          'feyb': this.state.feeChoices[this.state.feeValue][1],
          'note': (this.state.note || null),
        }
      }
      this.props.api.btcWalletCommand(command).then(res => this.setState({signing: true}));
    } else if (this.state.payeeType === 'address') {
      let command = {
        'init-payment-external': {
          'address': this.state.payee,
          'value': parseInt(this.state.satsAmount),
          'feyb': 1,
          'note': (this.state.note || null),
        }
      }
      this.props.api.btcWalletCommand(command).then(res => this.setState({signing: true}));
    }
  }

  render() {
    let payeeColor = "black";
    let payeeBg = "white";
    let payeeBorder = "lightGray";
    if (this.props.error) {
      payeeColor="red";
      payeeBorder = "red";
      payeeBg="veryLightRed";
    } else if (this.state.focusPayee && this.state.validPayee) {
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


    const { api, value, conversion, stopSending, denomination, psbt, currencyRates, error, network, fee } = this.props;
    const { denomAmount, satsAmount, signing, payee, choosingSignMethod, signMethod } = this.state;

    const signReady = (this.state.ready && (parseInt(this.state.satsAmount) > 0)) && !signing;

    let invoice = null;
    if (signMethod === 'Sign Transaction') {
      invoice =
        <Invoice
          network={network}
          api={api}
          psbt={psbt}
          fee={fee}
          currencyRates={currencyRates}
          stopSending={stopSending}
          payee={payee}
          denomination={denomination}
          satsAmount={satsAmount}
          state={this.props.state}
        />
    } else if (signMethod === 'Sign with Bridge') {
      invoice =
        <BridgeInvoice
          state={this.props.state}
          api={api}
          psbt={psbt}
          currencyRates={currencyRates}
          stopSending={stopSending}
          payee={payee}
          denomination={denomination}
          satsAmount={satsAmount}
        />
    }

    return (
      <>
        { (signing && psbt) ? invoice :
          <Col
            width='100%'
            backgroundColor='white'
            borderRadius='48px'
            mb={5}
            p={5}
          >
            <Col width="100%">
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
              {error &&
               <Row
                 alignItems='center'
                 justifyContent='space-between'>
                 {/* yes this is a hack */}
                 <Box width='calc(40% - 30px)'/>
                 <Error
                   error={error}
                   fontSize='14px'
                   ml={2}
                   mt={2}
                   width='100%' />
               </Row>
              }
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
              <Row mt={4} width="100%" justifyContent="space-between">
                <Text
                  gray
                  fontSize={1}
                  fontWeight='600'
                  width="40%"
                >Fee</Text>
                <Row alignItems="center">
                  <Text mr={2} color="lightGray" fontSize="14px">
                    {this.state.feeChoices[this.state.feeValue][1]} sats/vbyte
                  </Text>
                  <Icon icon="ChevronSouth"
                        fontSize="14px"
                        color="lightGray"
                        onClick={() => {if (!this.state.showModal) this.setState({showModal: true}); }}
                        cursor="pointer"/>
                </Row>
              </Row>
              <Col alignItems="center">
                {!this.state.showModal ? null :
                 <FeePicker
                   feeChoices={this.state.feeChoices}
                   feeSelect={this.feeSelect}
                   feeDismiss={this.feeDismiss}
                 />
                }
              </Col>
              <Row mt={4} width="100%"
                justifyContent="space-between"
                alignItems='center'
              >
                <Text
                  gray
                  fontSize={1}
                  fontWeight='600'
                  width="40%"
                >Note</Text>
                <Input
                  onFocus={() => {this.setState({focusNote: true})}}
                  onBlur={() => {this.setState({focusNote: false})}}
                  fontSize='14px'
                  width='100%'
                  placeholder="What's this for?"
                  type='text'
                  borderColor={this.state.focusNote ? "lightGray" : "none"}
                  disabled={signing}
                  value={this.state.note}
                  onChange={e => {
                    this.setState({
                      note: e.target.value,
                    });
                  }}
                />
              </Row>
            </Col>
            <Row
              flexDirection='row-reverse'
              alignItems="center"
              mt={4}
            >
              <Signer
                signReady={signReady}
                choosingSignMethod={choosingSignMethod}
                signMethod={signMethod}
                setSignMethod={this.setSignMethod}
                initPayment={this.initPayment} />
              { (!(signing && !error)) ? null :
                <LoadingSpinner mr={2} background="midOrange" foreground="orange"/>
              }
              <Button
                width='48px'
                children={
                  <Icon
                    icon={choosingSignMethod ? 'X' : 'Ellipsis'}
                    color={signReady ? 'blue' : 'lighterGray'}
                  />
                }
                fontSize={1}
                fontWeight='bold'
                borderRadius='24px'
                mr={2}
                py='24px'
                px='24px'
                onClick={() => this.toggleSignMethod(choosingSignMethod)}
                color={signReady ? 'white' : 'lighterGray'}
                backgroundColor={signReady ? 'rgba(33, 157, 255, 0.2)' : 'veryLightGray'}
                disabled={!signReady}
                border='none'
                style={{cursor: signReady ? 'pointer' : 'default'}} />
            </Row>
          </Col>
        }
      </>
    );
  }
}
