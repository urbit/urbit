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
      copied: false,
    }

    this.copyAddress = this.copyAddress.bind(this);
  }

  copyAddress() {
    let address = this.props.state.address;
    function listener(e) {
      e.clipboardData.setData('text/plain', address);
      e.preventDefault();
    }

    document.addEventListener('copy', listener);
    document.execCommand('copy');
    document.removeEventListener('copy', listener);

    this.props.api.btcWalletCommand({'gen-new-address': null});
    this.setState({copied: true});
    setTimeout(() => {
      this.setState({copied: false});
    }, 2000);
  }


  render() {
    const sats = (this.props.state.balance || 0);
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
           api={api}
           psbt={this.props.state.psbt}
           shipWallets={this.props.state.shipWallets}
           value={value}
           denomination={denomination}
           sats={sats}
           conversion={conversion}
           stopSending={() => {
             this.setState({sending: false});
             store.handleEvent({data: {psbt: ''}});
           }}
         /> :
         <Col
           height="400px"
           width='100%'
           backgroundColor="white"
           borderRadius="32px"
           justifyContent="space-between"
           mb={5}
           p={5}
         >
           <Row justifyContent="space-between">
             <Text color="orange" fontSize={1}>Balance</Text>
             <Text color="lighterGray" fontSize="14px" mono>{addressText}</Text>
             <CurrencyPicker
               denomination={denomination}
               currencies={this.props.state.currencyRates}
             />
           </Row>
           <Col justifyContent="center" alignItems="center">
             <Text fontSize="52px" color="orange">{value}</Text>
             <Text fontSize={1} color="orange">{sats} sats</Text>
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
                     py="24px"
                     px="24px"
                     onClick={() => this.setState({sending: true})}
             />
             <Button children={(this.state.copied) ? "Address Copied!" : "Copy Address"}
                     mr={3}
                     disabled={this.state.copied}
                     fontSize={1}
                     fontWeight="bold"
                     color={(this.state.copied) ? "green" : "orange"}
                     backgroundColor={(this.state.copied) ? "veryLightGreen" : "midOrange" }
                     style={{cursor: (this.state.copied) ? "default" : "pointer"}}
                     borderColor="none"
                     borderRadius="24px"
                     py="24px"
                     px="24px"
                     onClick={this.copyAddress}
             />
           </Row>
          </Col>
        }
      </>
    );
  }
}
