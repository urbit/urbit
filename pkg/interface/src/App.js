import { hot } from 'react-hot-loader/root';
import 'react-hot-loader';
import * as React from 'react';
import { BrowserRouter as Router, Route, withRouter, Switch } from 'react-router-dom';
import styled, { ThemeProvider, createGlobalStyle } from 'styled-components';
import { sigil as sigiljs, stringRenderer } from 'urbit-sigil-js';

import Mousetrap from 'mousetrap';
import 'mousetrap-global-bind';

import './css/indigo-static.css';
import './css/fonts.css';
import light from './themes/light';
import dark from './themes/old-dark';

import LaunchApp from './apps/launch/app';
import ChatApp from './apps/chat/app';
import DojoApp from './apps/dojo/app';
import GroupsApp from './apps/groups/app';
import LinksApp from './apps/links/app';
import PublishApp from './apps/publish/app';
import Profile from './apps/profile/profile';

import StatusBar from './components/StatusBar';
import Omnibox from './components/Omnibox';
import ErrorComponent from './components/Error';

import GlobalStore from './store/store';
import GlobalSubscription from './subscription/global';
import GlobalApi from './api/global';
import { uxToHex } from './lib/util';
import { Sigil } from './lib/sigil';

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
  height: 100%;
  width: 100%;
  padding: 0;
  margin: 0;
  ${p => p.background?.type === 'url' ? `
    background-image: url('${p.background?.url}');
    background-size: cover;
    ` : p.background?.type === 'color' ? `
    background-color: ${p.background.color}
    ` : ``
  }
`;

const Content = styled.div`
   height: calc(100% - 45px);
`;

const StatusBarWithRouter = withRouter(StatusBar);

class App extends React.Component {
  constructor(props) {
    super(props);
    this.ship = window.ship;
    this.store = new GlobalStore();
    this.store.setStateHandler(this.setState.bind(this));
    this.state = this.store.state;

    this.appChannel = new window.channel();
    this.api = new GlobalApi(this.ship, this.appChannel, this.store);
    this.subscription =
      new GlobalSubscription(this.store, this.api, this.appChannel);

    this.updateTheme = this.updateTheme.bind(this);
    this.setFavicon = this.setFavicon.bind(this);
  }

  componentDidMount() {
    this.subscription.start();
    this.themeWatcher = window.matchMedia('(prefers-color-scheme: dark)');
    this.api.local.setDark(this.themeWatcher.matches);
    this.themeWatcher.addListener(this.updateTheme);
    this.api.local.getBaseHash();
    this.store.rehydrate();
    Mousetrap.bindGlobal(['command+/', 'ctrl+/'], (e) => {
      e.preventDefault();
      this.api.local.setOmnibox();
    });
    this.setFavicon();
  }

  componentWillUnmount() {
    this.themeWatcher.removeListener(this.updateTheme);
  }

  componentDidUpdate(prevProps, prevState, snapshot) {
    this.setFavicon();
  }

  updateTheme(e) {
    this.api.local.setDark(e.matches);
  }

  setFavicon() {
    if (window.ship.length < 14) {
      let background = '#ffffff';
      if (this.state.contacts.hasOwnProperty('/~/default')) {
        background = `#${uxToHex(this.state.contacts['/~/default'][window.ship].color)}`;
      }
      const foreground = Sigil.foregroundFromBackground(background);
      const svg = sigiljs({
        patp: window.ship,
        renderer: stringRenderer,
        size: 16,
        colors: [background, foreground]
      });
      const dataurl = 'data:image/svg+xml;base64,' + btoa(svg);
      const favicon = document.querySelector('[rel=icon]');
      favicon.href = dataurl;
      favicon.type = 'image/svg+xml';
    }
  }

  render() {
    const channel = window.channel;

    const associations = this.state.associations ? this.state.associations : { contacts: {} };
    const { state } = this;
    const theme = state.dark ? dark : light;
    const { background } = state;

    return (
      <ThemeProvider theme={theme}>
        <Root background={background} >
          <Router>
            <StatusBarWithRouter
              props={this.props}
              associations={associations}
              invites={this.state.invites}
              api={this.api}
              connection={this.state.connection}
              subscription={this.subscription}
              ship={this.ship}
            />
            <Omnibox
              associations={state.associations}
              apps={state.launch}
              api={this.api}
              dark={state.dark}
              show={state.omniboxShown}
            />
            <Content>
              <Switch>
                <Route
                  exact
                  path='/'
                  render={p => (
                    <LaunchApp
                      ship={this.ship}
                      api={this.api}
                      {...state}
                      {...p}
                    />
                  )}
                />
                <Route
                  path='/~chat'
                  render={p => (
                    <ChatApp
                      ship={this.ship}
                      api={this.api}
                      subscription={this.subscription}
                      {...state}
                      {...p}
                    />
                  )}
                />
                <Route
                  path='/~dojo'
                  render={p => (
                    <DojoApp
                      ship={this.ship}
                      channel={channel}
                      subscription={this.subscription}
                      {...p}
                    />
                  )}
                />
                <Route
                  path='/~groups'
                  render={p => (
                    <GroupsApp
                      ship={this.ship}
                      api={this.api}
                      subscription={this.subscription}
                      {...state}
                      {...p}
                    />
                  )}
                />
                <Route
                  path='/~link'
                  render={p => (
                    <LinksApp
                      ship={this.ship}
                      api={this.api}
                      subscription={this.subscription}
                      {...state}
                      {...p}
                    />
                  )}
                />
                <Route
                  path='/~publish'
                  render={p => (
                    <PublishApp
                      ship={this.ship}
                      api={this.api}
                      subscription={this.subscription}
                      {...state}
                      {...p}
                    />
                  )}
                />
              <Route
                path="/~profile"
                render={ p => (
                  <Profile ship={this.ship} api={this.api} {...state} />
                )}
              />
              <Route
                render={(props) => (
                  <ErrorComponent {...props} code={404} description="Not Found" />
                )}
              />
              </Switch>
            </Content>
          </Router>
        </Root>
      </ThemeProvider>
    );
  }
}


export default process.env.NODE_ENV === 'production' ? App : hot(App);

