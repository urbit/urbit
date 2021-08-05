import React from 'react';
import ReactDOM from 'react-dom';
import { Root } from './js/components/root.js';
import { api } from './js/api.js';
import { subscription } from "./js/subscription.js";

import './css/indigo-static.css';
import './css/fonts.css';
import './css/custom.css';
import './channel';

// rebuild x3

const channel = new window.channel();
api.setChannel(window.ship, channel);


if (module.hot) {
  module.hot.accept()
}

ReactDOM.render((
  <Root channel={channel}/>
), document.querySelectorAll("#root")[0]);
