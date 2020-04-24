import * as React from "react";
import { BrowserRouter as Router, Route, Switch, Link, useParams, useRouteMatch, withRouter } from "react-router-dom";
import styled, { ThemeProvider, createGlobalStyle } from "styled-components";
import { cssReset, light, Col, Text, Row } from "@tlon/indigo-react";
// import { api } from '/api';
// import { store } from '/store';
// import { subscription } from "/subscription";

api.setAuthTokens({
  ship: window.ship
});

subscription.start();

import Chat from './apps/chat/Chat';

const Style = createGlobalStyle`
  ${cssReset}
  html {
    background-color: ${p => p.theme.colors.white};
  }

  strong {
    font-weight: 600;
  }
`

const Root = styled.div`
  font-family: ${p => p.theme.fonts.sans};
  line-height: ${p => p.theme.lineHeights.regular};
`;

const Home = () => (
  <div>
    Home
    <Link to='/~chat'>Chat</Link>
    <Link to={`/~chat/new/dm/~zod`}>new/dm/~zod</Link>
    <Link to='/~publish'>Publish</Link>
  </div>
)

const StatusBar = (props) => {
  let routeTitle
  // should be encapsulated
  if (props.location.pathname === '/') routeTitle = 'Home'
  if (props.location.pathname === '/~chat') routeTitle = 'Chat'
  return (
    <Row p='6' expand> STATUS BAR - {routeTitle} </Row>
  )
}
const StatusBarWithRouter = withRouter(StatusBar)


export default class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {

    };
  }

  render() {
    const { state } = this

    return (
      <ThemeProvider theme={light}>
        <Style/>
        <Root>
          HELLO
          <Router>
            <StatusBarWithRouter props={this.props}/>
            <div>
              <Route exact path="/" component={Home} />
              <Route path="/~chat" component={Chat} />
            </div>
          </Router>
        </Root>
      </ThemeProvider>
    );
  }
}
