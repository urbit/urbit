import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import classnames from 'classnames';
import { api } from '/api';
import { store } from '/store';
import { Switch } from 'react-router';

export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  render() {
    return (
      <p>{this.state.path}</p>
    );
  }
}
