import React from 'react';
import ReactDOM from 'react-dom';
import Root from './js/components/root.js';
import { api } from './js/api.js';
import { SettingsProvider } from './js/hooks/useSettings';

import './css/indigo-static.css';
import './css/fonts.css';
import './css/custom.css';

// rebuild x3

const channel = new window.channel();
api.setChannel(window.ship, channel);

if (module.hot) {
  module.hot.accept();
}

ReactDOM.render(
  <SettingsProvider channel={channel}>
    <Root />
  </SettingsProvider>,
  document.querySelectorAll('#root')[0]
);
