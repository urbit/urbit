import { Box, Col } from '@tlon/indigo-react';
import React, { Component } from 'react';
import Helmet from 'react-helmet';
import { Route } from 'react-router-dom';
import withState from '~/logic/lib/withState';
import useHarkState from '~/logic/state/hark';
import Api from './api';
import { History } from './components/history';
import { Input } from './components/input';
import './css/custom.css';
import Store from './store';
import Subscription from './subscription';

class TermApp extends Component {
  store: Store;
  api: any;
  subscription: any;
  props: any;
  state: any;
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
    const channel = new (window as any).channel();
    this.api = new Api(this.props.ship, channel);
    this.store.api = this.api;

    this.subscription = new Subscription(this.store, this.api, channel);
    this.subscription.start();
  }

  componentWillUnmount() {
    this.subscription.delete();
    this.store.clear();
    this.resetControllers();
  }

  render() {
    return (
      <>
        <Helmet defer={false}>
          <title>{ this.props.notificationsCount ? `(${String(this.props.notificationsCount) }) `: '' }Landscape</title>
        </Helmet>
        <Box
          height='100%'
        >
          <Route
            exact
            path="/~term/"
            render={(props) => {
              return (
                <Box
                  width='100%'
                  height='100%'
                  display='flex'
                >
                  <Col
                    p={3}
                    backgroundColor='white'
                    width='100%'
                    minHeight={0}
                    minWidth={0}
                    color='lightGray'
                    borderRadius={2}
                    mx={['0','3']}
                    mb={['0','3']}
                    border={['0','1']}
                    cursor='text'
                  >
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
              );
            }}
          />
        </Box>
      </>
    );
  }
}

export default withState(TermApp, [[useHarkState]]);
