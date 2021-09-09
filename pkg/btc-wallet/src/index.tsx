import React from 'react';
import ReactDOM from 'react-dom';
import { api } from './lib/api';
import { SettingsProvider } from './hooks/useSettings';
import App from './App';

import './css/indigo-static.css';
import './css/fonts.css';
import './css/custom.css';

const channel = new (window as any).channel();
api.setChannel((window as any).ship, channel);

if (module.hot) {
  module.hot.accept();
}

ReactDOM.render(
  <SettingsProvider channel={channel}>
    <App />
  </SettingsProvider>,
  document.querySelectorAll('#root')[0]
);
