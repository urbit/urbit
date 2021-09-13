import React, { Component } from 'react';
import {
  Box,
  Icon,
  Row,
  Text,
  LoadingSpinner,
  Col,
} from '@tlon/indigo-react';
import {
  Switch,
  Route,
} from 'react-router-dom';
import Balance      from './balance.js';
import Transactions from './transactions.js';
import Warning      from './warning.js';
import Header       from './header.js';
import Settings     from './settings.js';

export default class Body extends Component {
  constructor(props) {
    super(props);
  }

  render() {

    const cardWidth = window.innerWidth <= 475 ? '350px' : '400px'

    if (!this.props.loaded) {
      return (
        <Box display="flex" width="100%" height="100%" alignItems="center" justifyContent="center">
          <LoadingSpinner
            width={7}
            height={7}
            background="midOrange"
            foreground="orange"
          />
        </Box>
      );
    } else {
      return (
        <Switch>
          <Route path="/settings">
            <Col
             display='flex'
             flexDirection='column'
             width={cardWidth}
            >
              <Header settings={true} state={this.props.state}/>
              <Settings state={this.props.state}
                api={this.props.api}
                network={this.props.network}
              />
            </Col>
          </Route>
          <Route path="/">
            <Col
             display='flex'
             flexDirection='column'
             width={cardWidth}
            >
              <Header settings={false} state={this.props.state}/>
              { (!this.props.warning) ? null : <Warning api={this.props.api}/>}
              <Balance api={this.props.api} state={this.props.state} network={this.props.network}/>
              <Transactions state={this.props.state} network={this.props.network}/>
            </Col>
          </Route>
        </Switch>
      );
    }
  }
}
