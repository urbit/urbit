import React from 'react';
import ReactDOM from 'react-dom';
import { Root } from '/components/root';
import { api } from '/api';
import { store } from '/store';
import { subscription } from "/subscription";

api.setAuthTokens({
  ship: window.ship
});

subscription.start();

ReactDOM.render((
  <Root />
), document.querySelectorAll("#root")[0]);
