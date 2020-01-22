import React from 'react';
import ReactDOM from 'react-dom';
import { Root } from '/components/root';
import { HeaderBar } from '/components/lib/header-bar.js';
import { api } from '/api';
import { store } from '/store';
import { subscription } from "/subscription";

api.setAuthTokens({
  ship: window.ship
});

subscription.start();

ReactDOM.render((
  <HeaderBar />
), document.getElementById("header"));

ReactDOM.render((
  <Root />
), document.querySelectorAll("#root")[0]);
