import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import _ from 'lodash';
import { api } from '../api.js';
import { store } from '../store.js';

import styled, { ThemeProvider, createGlobalStyle } from 'styled-components';
import light from './themes/light';
import dark from './themes/dark';
import { Text, Box } from '@tlon/indigo-react';
import StartupModal from './lib/startupModal.js';
import Header from './lib/header.js'
import Balance from './lib/balance.js'
import Transactions from './lib/transactions.js'


export class Root extends Component {
  constructor(props) {
    super(props);
    this.ship = window.ship;
    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  componentDidUpdate(){
    console.log('state', this.state);

  }

  render() {
    return (
      <BrowserRouter>
        <ThemeProvider theme={light}>
        <StartupModal api={api} state={this.state}/>
        <Box display='flex'
          flexDirection='column'
          position='absolute'
          alignItems='center'
          backgroundColor='lightOrange'
          height='100%'
          width='100%'
          px={[0,4]}
          pb={[0,4]}
        >
          <Box
           display='flex'
           flexDirection='column'
           height='100%'
           width='400px'
          >
            <Header />
            <Balance api={api} state={this.state}/>
            <Transactions state={this.state}/>
          </Box>
        </Box>
        </ThemeProvider>
      </BrowserRouter>
    )
  }
}

