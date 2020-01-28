import React from 'react';
import ReactDOM from 'react-dom';
import { HeaderBar } from '/components/lib/header-bar';
import { Root } from '/components/root';
import { api } from '/api';
import { subscription } from "/subscription";

api.setAuthTokens({
  ship: window.ship,
  dojoId: "soto-" + Math.random().toString(36).substring(2),
});

subscription.start();

ReactDOM.render(<HeaderBar />, document.getElementById("header"));

ReactDOM.render((
  <Root />
), document.querySelectorAll("#root")[0]);
