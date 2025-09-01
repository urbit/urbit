// import React from 'react'
// import ReactDOM from 'react-dom/client'
// import App from './App.tsx'
// import './index.css'

// ReactDOM.createRoot(document.getElementById('root')!).render(
//   <React.StrictMode>
//     <App />
//   </React.StrictMode>,
// )

import React from 'react';
import ReactDOM from 'react-dom';
import { Root } from './js/components/root';
import { api } from './js/api';
import { store } from './js/store';
import { subscription } from "./js/subscription";

import './index.css';

api.setAuthTokens({
  //@ts-ignore //TODO
  ship: window.ship
});

console.log('new world!');

//@ts-ignore //TODO
window.urb = new window.channel();
subscription.start();

ReactDOM.render((
  <Root />
), document.querySelectorAll("#root")[0]);

