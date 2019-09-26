import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import classnames from 'classnames';
import { api } from '/api';
import { store } from '/store';
import { Switch } from 'react-router';

import { Dom } from '/components/dom';
import { TestComp } from '/components/test';


export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;

    store.setStateHandler(this.setState.bind(this));
  }

  render() {
    return (
      <div className="w-100 h-100 overflow-y-scroll overflow-x-scroll">
        <Dom status={this.state.status}
          body={this.state.data}
          api={api}/>
      </div>
    );
  }
}
