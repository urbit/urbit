import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import _ from 'lodash';
import { api } from '../api.js';
import { store } from '../store.js';

import styled, { ThemeProvider, createGlobalStyle } from 'styled-components';
import light from './themes/light';
import dark from './themes/dark';
import {
  Text,
  Box,
  Reset,
  Col,
  LoadingSpinner,
} from '@tlon/indigo-react';
import StartupModal from './lib/startupModal.js';
import Header from './lib/header.js'
import Balance from './lib/balance.js'
import Transactions from './lib/transactions.js'
import Warning from './lib/warning.js'
import Body from './lib/body.js'
import { subscription } from '../subscription.js'

const network = "bitcoin";

export class Root extends Component {
  constructor(props) {
    super(props);
    this.ship = window.ship;
    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  componentDidMount(){
    this.props.channel.setOnChannelError((e) => {
      subscription.start();
    });
    subscription.start();
  }

  render() {
    const loaded = this.state.loaded;
    const warning = this.state.showWarning;
    const blur = (!loaded) ? false :
      !(this.state.wallet && this.state.provider);

    return (
      <BrowserRouter>
        <ThemeProvider theme={light}>
        <Reset/>
        { (loaded) ? <StartupModal api={api} state={this.state} network={network}/> : null }
          <Box display="flex"
            flexDirection='column'
            position='absolute'
            alignItems='center'
            backgroundColor='lightOrange'
            width='100%'
            minHeight={loaded ? '100%' : 'none'}
            height={loaded ? 'none' : '100%'}
            style={{filter: (blur ? 'blur(8px)' : 'none')}}
            px={[0,4]}
            pb={[0,4]}
          >
            <Body loaded={loaded}
              state={this.state}
              api={api} network={network}
              warning={warning}
            />
          </Box>
        </ThemeProvider>
      </BrowserRouter>
    )
  }
}
