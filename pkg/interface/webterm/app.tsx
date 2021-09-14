import { Box, Col } from '@tlon/indigo-react';
import React, { Component } from 'react';
import dark from '@tlon/indigo-dark';
import light from '@tlon/indigo-light';
import { ThemeProvider } from 'styled-components';
import Api from './api';
import { History } from './components/history';
import { Input } from './components/input';
import './css/custom.css';
import Store from './store';
import Subscription from './subscription';
import Channel from './lib/channel';

class TermApp extends Component<any, any> {
  store: Store;
  api: any;
  subscription: any;
  constructor(props) {
    super(props);
    this.store = new Store();
    this.store.setStateHandler(this.setState.bind(this));

    this.state = this.store.state;
  }

  resetControllers() {
    this.api = null;
    this.subscription = null;
  }

  componentDidMount() {
    this.resetControllers();
    // eslint-disable-next-line new-cap
    const channel = new Channel();
    this.api = new Api(window.ship, channel);
    this.store.api = this.api;

    this.subscription = new Subscription(this.store, this.api, channel);
    this.subscription.start();
  }

  componentWillUnmount() {
    this.subscription.delete();
    this.store.clear();
    this.resetControllers();
  }

  getTheme() {
    const { props } = this;
    return ((props.dark && props?.display?.theme == 'auto') ||
      props?.display?.theme == 'dark'
    ) ? dark : light;
  }

  render() {
    const theme = this.getTheme();
    return (
      <ThemeProvider theme={theme}>
        <Box
          width='100%'
          height='100%'
          p={['0','3']}
          style={{ boxSizing: 'border-box' }}
        >
          <Col
            p={3}
            backgroundColor='white'
            width='100%'
            height='100%'
            minHeight={0}
            minWidth={0}
            color='lightGray'
            borderRadius={2}
            border={['0','1']}
            cursor='text'
            style={{ boxSizing: 'border-box' }}
          >
            {/* @ts-ignore declare props in later pass */}
            <History log={this.state.lines.slice(0, -1)} />
            <Input
              ship={this.props.ship}
              cursor={this.state.cursor}
              api={this.api}
              store={this.store}
              line={this.state.lines.slice(-1)[0]}
            />
          </Col>
        </Box>
      </ThemeProvider>
    );
  }
}

export default TermApp;
