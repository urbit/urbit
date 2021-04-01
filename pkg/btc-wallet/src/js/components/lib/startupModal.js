import React, { Component } from 'react';
import { Box } from '@tlon/indigo-react';

import WalletModal from './walletModal.js'
import ProviderModal from './providerModal.js'


export default class StartupModal extends Component {
  constructor(props) {
    super(props);
  }


  render() {
    let modal = null;

    if (this.props.state.hasWallet && this.props.state.provider) {
      return null;
    } else if (!this.props.state.provider){
      modal =
        <ProviderModal
          api={this.props.api}
          providerPerms={this.props.state.providerPerms}
        />
    } else if (!this.props.state.hasWallet){
      modal = <WalletModal api={this.props.api}/>
    }
    return (
      <Box
        backgroundColor="scales.black20"
        left="0px"
        top="0px"
        width="100%"
        height="100%"
        position="fixed"
        display="flex"
        zIndex={10}
        justifyContent="center"
        alignItems="center"
      >
        <Box display="flex"
          flexDirection="column"
          width='400px'
          backgroundColor="white"
          borderRadius={3}
        >
          {modal}
        </Box>
      </Box>
    );
  }
}
