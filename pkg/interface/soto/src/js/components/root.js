import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import classnames from 'classnames';
import _ from 'lodash';
import { HeaderBar } from './lib/header-bar';
import { History } from './history';
import { Input } from './input';
import { api } from '../api';
import { store } from '../store';

export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  render() {
    return (
      <BrowserRouter>
        <div style={{height: "100vh"}}>
        <HeaderBar/>
        <Route exact path="/~soto" render={ () => {
          return (
            <div className="pa3 bg-black mono gray3 w-100"
            style={{lineHeight: "1.4", height: "calc(100% - 48px)", cursor: "text"}}>
              <History commandLog={this.state.txt}/>
              <Input ship={ship} cursor={this.state.cursor} prompt={this.state.prompt} input={this.state.input}/>
            </div>
          )}}
        />
        </div>
      </BrowserRouter>
    )
  }
}