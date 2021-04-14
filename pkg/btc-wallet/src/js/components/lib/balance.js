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

function currencyFormat(sats, conversion, denomination) {
  let val;
  let text;
  switch (denomination) {
    case "USD":
    case "CAD":
      val = sats * 0.00000001 * conversion[denomination]
      text = '$' + val.toFixed(2).replace(/(\d)(?=(\d{3})+(?!\d))/g, '$1,')
      break;
    case "BTC":
      val = sats * 0.00000001;
      text =  'BTC ' + val;
      break;
    default:
      break;
  }
  return text;
}

export default class Balance extends Component {
  constructor(props) {
    super(props);

    this.state = {
      conversion: {
        USD: 50000,
        CAD: 70000,
        BTC: 1,
      },
      denomination: "USD",
      sending: false,
    }
  }


  render() {
    const sats = (this.props.state.wallet) ?
      (this.props.state.wallet.balance || 0) : 0;
    const value = currencyFormat(sats, this.state.conversion, this.state.denomination);

    return (
      <Col
        height="400px"
        width='100%'
        backgroundColor="white"
        borderRadius="32px"
        mb={5}
        p={5}
      >
        {this.state.sending ?
         <Send
           api={api}
           value={value}
           denomination={this.state.denomination}
           sats={sats}
           conversion={this.state.conversion}
           stopSending={() => {this.setState({sending: false})}}
         /> :
         <>
           <Row justifyContent="space-between">
             <Text color="orange" fontSize={1}>Balance</Text>
             <Text color="lighterGray" fontSize="14px">bc1qxy...hx0wlh</Text>
             <Row>
               <Icon icon="ChevronDouble" color="orange" pt="2px"/>
               <Text color="orange" fontSize={1}>{this.state.denomination}</Text>
             </Row>
           </Row>
           <Col justifyContent="center" alignItems="center" mt="100px" mb="100px">
             <Text fontSize="52px" color="orange">{value}</Text>
             <Text fontSize={1} color="orange">{sats} sats</Text>
           </Col>
           <Row flexDirection="row-reverse">
             <Button children="Send"
                     fontSize={1}
                     fontWeight="bold"
                     color="lighterGray"
                     backgroundColor="veryLightGray"
                     borderColor="none"
                     borderRadius="24px"
                     py="24px"
                     px="24px"
                     onClick={() => this.setState({sending: true})}
             />
             <Button children="Copy Address" mr={3}
                     fontSize={1}
                     fontWeight="bold"
                     color="orange"
                     backgroundColor="midOrange"
                     borderColor="none"
                     borderRadius="24px"
                     py="24px"
                     px="24px"
             />
           </Row>
         </>
        }
      </Col>
    );
  }
}
