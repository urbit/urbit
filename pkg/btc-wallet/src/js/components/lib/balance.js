import React, { Component } from 'react';
import {
  Box,
  Icon,
  Row,
  Text,
  Button,
  Col,
} from '@tlon/indigo-react';

import Send from './send.js'
import CurrencyPicker from './currencyPicker.js'
import { currencyToSats, satsToCurrency } from '../../lib/util.js';


export default class Balance extends Component {
  constructor(props) {
    super(props);

    this.state = {
      sending: false,
      copiedButton: false,
      copiedString: false,
    }

    this.copyAddress = this.copyAddress.bind(this);
  }

  copyAddress(arg) {
    let address = this.props.state.address;
    navigator.clipboard.writeText(address);
    this.props.api.btcWalletCommand({'gen-new-address': null});

    if (arg === 'button'){
      this.setState({copiedButton: true});
      setTimeout(() => { this.setState({copiedButton: false}); }, 2000);
    } else if (arg === 'string') {
      this.setState({copiedString: true});
      setTimeout(() => { this.setState({copiedString: false}); }, 2000);
    }
  }


  render() {
    const sats = (this.props.state.confirmedBalance || 0);
    const unconfirmedSats = this.props.state.unconfirmedBalance;

    const unconfirmedString = unconfirmedSats ? ` (${unconfirmedSats}) ` : '';

    const denomination = this.props.state.denomination;
    const value = satsToCurrency(sats, denomination, this.props.state.currencyRates);
    const sendDisabled = (sats === 0);
    const addressText = (this.props.state.address === null) ? '' :
      this.props.state.address.slice(0, 6) + '...' +
      this.props.state.address.slice(-6);

    const conversion = this.props.state.currencyRates[denomination].last;

    return (
      <>
        {this.state.sending ?
         <Send
           state={this.props.state}
           api={api}
           psbt={this.props.state.psbt}
           fee={this.props.state.fee}
           currencyRates={this.props.state.currencyRates}
           shipWallets={this.props.state.shipWallets}
           value={value}
           denomination={denomination}
           sats={sats}
           conversion={conversion}
           network={this.props.network}
           error={this.props.state.error}
           stopSending={() => {
             this.setState({sending: false});
             store.handleEvent({data: {psbt: '', fee: 0, error: '', "broadcast-fail": null}});
           }}
         /> :
         <Col
           height="400px"
           width='100%'
           backgroundColor="white"
           borderRadius="48px"
           justifyContent="space-between"
           mb={5}
           p={5}
         >
           <Row justifyContent="space-between">
             <Text color="orange" fontSize={1}>Balance</Text>
             <Text color="lightGray" fontSize="14px" mono style={{cursor: "pointer"}}
                   onClick={() => {this.copyAddress('string')}}>
              {this.state.copiedString ? "copied" : addressText}
             </Text>
             <CurrencyPicker
               api={this.props.api}
               denomination={denomination}
               currencies={this.props.state.currencyRates}
             />
           </Row>
           <Col justifyContent="center" alignItems="center">
             <Text fontSize="40px" color="orange" style={{whiteSpace: "nowrap"}} >
               {value}
             </Text>
             <Text fontSize={1} color="orange">{`${sats}${unconfirmedString} sats`}</Text>
           </Col>
           <Row flexDirection="row-reverse">
             <Button children="Send"
                     disabled={sendDisabled}
                     fontSize={1}
                     fontWeight="bold"
                     color={sendDisabled ? "lighterGray" : "white"}
                     backgroundColor={sendDisabled ? "veryLightGray" : "orange"}
                     style={{cursor: sendDisabled ? "default" : "pointer" }}
                     borderColor="none"
                     borderRadius="24px"
                     height="48px"
                     onClick={() => this.setState({sending: true})}
             />
             <Button children={(this.state.copiedButton) ? "Address Copied!" : "Copy Address"}
                     mr={3}
                     disabled={this.state.copiedButton}
                     fontSize={1}
                     fontWeight="bold"
                     color={(this.state.copiedButton) ? "green" : "orange"}
                     backgroundColor={(this.state.copiedButton) ? "veryLightGreen" : "midOrange" }
                     style={{cursor: (this.state.copiedButton) ? "default" : "pointer"}}
                     borderColor="none"
                     borderRadius="24px"
                     height="48px"
                     onClick={() => {this.copyAddress('button')}}
             />
           </Row>
          </Col>
        }
      </>
    );
  }
}
