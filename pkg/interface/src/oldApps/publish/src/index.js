import React from 'react';
import ReactDOM from 'react-dom';
import { Root } from './js/components/root';
import { subscription } from "./js/subscription";

api.setAuthTokens({
  ship: window.ship
});

subscription.start();

ReactDOM.render((
  <Root />
), document.querySelectorAll("#root")[0]);
