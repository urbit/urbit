import * as React from 'react';
import { BrowserRouter as Router, Route, Link, withRouter } from 'react-router-dom';
import styled, { ThemeProvider, createGlobalStyle } from 'styled-components';
import { cssReset, light } from '@tlon/indigo-react';

import ChatApp from './apps/chat/ChatApp';
import StatusBar from './components/StatusBar';

const Style = createGlobalStyle`
  ${cssReset}
  html {
    background-color: ${p => p.theme.colors.white};
  }

  strong {
    font-weight: 600;
  }
`;

const Root = styled.div`
  font-family: ${p => p.theme.fonts.sans};
  line-height: ${p => p.theme.lineHeights.regular};
`;

const Home = () => (
  <div>
    Home
    <Link to='/~chat'>Chat</Link>
    <Link to={'/~chat/new/dm/~zod'}>new/dm/~zod</Link>
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
        <Style />
        <Root>
          <Router>
            <StatusBarWithRouter props={this.props} />
            <div>
              <Route exact path="/" component={Home} />
              <Route path="/~chat" render={
                p => <ChatApp ship={this.ship} channel={this.channel} {...p} />
              }
              />
            </div>
          </Router>
        </Root>
      </ThemeProvider>
    );
  }
}
