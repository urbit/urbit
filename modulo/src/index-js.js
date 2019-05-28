import React from 'react';
import ReactDOM from 'react-dom';

import Wrapper from '/wrapper';
import { subscription } from "/subscription";

subscription.setAuthTokens({
  ship: window.ship
});

var css = document.createElement('link');
css.setAttribute('rel', 'stylesheet');
css.setAttribute('href', '/~chess/chessboard-css.css');
document.head.appendChild(css);

var script = document.createElement('script');
script.type = 'text/javascript';
script.setAttribute('src','/~chess/chessboard-js.js');
script.async = true;
script.onload = () => {
  let App = window.app;
  ReactDOM.render((
    <Wrapper>
      <App />
    </Wrapper>
  ), document.querySelectorAll("#root")[0]);

};

document.head.appendChild(script);
