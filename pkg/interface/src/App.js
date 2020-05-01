import * as React from 'react';
import { BrowserRouter as Router, Route, Link, withRouter } from 'react-router-dom';
import styled, { ThemeProvider, createGlobalStyle } from 'styled-components';
import './css/indigo-static.css';
import './css/fonts.css';
import { light } from '@tlon/indigo-react';

import ChatApp from './apps/chat/ChatApp';
import DojoApp from './apps/dojo/DojoApp';
import StatusBar from './components/StatusBar';

// const Style = createGlobalStyle`
//   ${cssReset}
//   html {
//     background-color: ${p => p.theme.colors.white};
//   }
//
//   strong {
//     font-weight: 600;
//   }
// `;

const Root = styled.div`
  font-family: ${p => p.theme.fonts.sans};
  line-height: ${p => p.theme.lineHeights.regular};
  max-height: 100vh;
  min-height: 100vh;
`;

const Home = () => (
  <div>
    Home
    <Link to='/~chat'>Chat</Link>
    <Link to='/~dojo'>Dojo</Link>
    <Link to='/~publish'>Publish</Link>
  </div>
);

const StatusBarWithRouter = withRouter(StatusBar);

export default class App extends React.Component {
  constructor(props) {
    super(props);
    this.channel = window.urb;
    this.ship = window.ship;
    this.state = {

    };
  }

  render() {
    return (
      <ThemeProvider theme={light}>
        <Root>
          <Router>
            <StatusBarWithRouter props={this.props} />
            <div>
              <Route exact path="/" component={Home} />
              <Route path="/~chat" render={
                p => <ChatApp ship={this.ship} channel={this.channel} {...p} />
              }
              />
              <Route path="/~dojo" render={
                p => <DojoApp ship={this.ship} channel={this.channel} {...p} />
              }
              />
            </div>
          </Router>
        </Root>
      </ThemeProvider>
    );
  }
}
