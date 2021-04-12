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

export default class Send extends Component {
  constructor(props) {
    super(props);

    this.state = {
      signing: false,
      denomAmount: '0.00',
      satsAmount: '0.00',
      payee: '',
    };

    this.initPayment  = this.initPayment.bind(this);
  }

  initPayment() {
    let command = {
      'init-payment': {
        'payee': this.state.payee,
        'value': parseInt(this.state.satsAmount),
        'feyb': 100,
      }
    }
    this.props.api.btcWalletCommand(command).then(
      (res) => console.log({res})
    );
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
        <>
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
            <Text fontSize={1}>To</Text>
            <Input
              width='auto'
              placeholder='~sampel-palnet or BTC address'
              value={payee}
              onChange={e => {
                this.setState({payee: e.target.value});
              }}
            />
          </Row>
          <Row
            alignItems='center'
            mt={2}
            justifyContent='space-between'>
            <Text fontSize={1}>Amount</Text>
            <Input
              type='number'
              border='none'
              width='auto'
              value={denomAmount}
              onChange={e => {
                this.setState({
                  denomAmount: e.target.value,
                  satsAmount: parseFloat(e.target.value) * conversion[denomination]
                });
              }}
            />
            <Text gray fontSize={1}>{denomination}</Text>
          </Row>
          <Row
            alignItems='center'
            mt={2}
            justifyContent='space-between'>
            <Input
              type='number'
              border='none'
              width='auto'
              value={satsAmount}
              onChange={e => {
                this.setState({
                  denomAmount: parseFloat(e.target.value) / conversion[denomination],
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
              children='Sign Transaction' mr={3}
              fontSize={1}
              color='white'
              backgroundColor='blue'
              borderColor='none'
              borderRadius='24px'
              py='24px'
              px='24px'
              onClick={() =>{
                this.initPayment()
                this.setState({signing: true})
              }}
            />
          </Row>

        </>
      }
      </>
    );
  }
}
