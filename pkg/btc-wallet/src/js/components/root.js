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
import { subscription } from '../subscription.js'


export class Root extends Component {
  constructor(props) {
    super(props);
    this.ship = window.ship;
    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
    console.log('state', this.state);
  }

  componentDidMount(){
    this.props.channel.setOnChannelError((e) => {
      subscription.start();
    });
    subscription.start();
  }

  render() {
    const loaded = this.state.loaded;;

    return (
      <BrowserRouter>
        <ThemeProvider theme={light}>
        <Reset/>
        { (loaded) ? <StartupModal api={api} state={this.state}/> : null }
        <Box display="flex"
          flexDirection='column'
          position='absolute'
          alignItems='center'
          backgroundColor='lightOrange'
          width='100%'
          minHeight={loaded ? '100%' : 'none'}
          height={loaded ? 'none' : '100%'}
          px={[0,4]}
          pb={[0,4]}
        >
          { (loaded) ?
              <Col
               display='flex'
               flexDirection='column'
               width='400px'
              >
                <Header />
                <Balance api={api} state={this.state}/>
                <Transactions state={this.state}/>
              </Col>
            :  <Box display="flex" width="100%" height="100%" alignItems="center" justifyContent="center">
                 <LoadingSpinner
                   width={7}
                   height={7}
                   background="midOrange"
                   foreground="orange"
                 />
               </Box>
          }
        </Box>
        </ThemeProvider>
      </BrowserRouter>
    )
  }
}
