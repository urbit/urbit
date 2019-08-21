import React from 'react';
import ReactDOM from 'react-dom';
import { Root } from '/components/root';
import { api } from '/api';
import { subscription } from "/subscription";
import * as util from '/lib/util';
import _ from 'lodash';

console.log('app running');

api.setAuthTokens({
  ship: window.ship
});

subscription.start();

window.util = util;
window._ = _;

ReactDOM.render((
  <Root />
), document.querySelectorAll("#root")[0]);
