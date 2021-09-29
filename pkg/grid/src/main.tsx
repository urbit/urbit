import React from 'react';
import ReactDOM from 'react-dom';
import { App } from './app';
import './styles/index.css';

ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById('app')
);

/* && import.meta.env.MODE !== 'development' */
if ('serviceWorker' in navigator) {
  window.addEventListener('load', () => {
    navigator.serviceWorker
      .register('/apps/grid/serviceworker.js', {
        scope: '/'
      })
      .then(() => {});
  });
}
