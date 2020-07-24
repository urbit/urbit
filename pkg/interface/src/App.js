import { hot } from 'react-hot-loader/root';
import 'react-hot-loader';
import * as React from 'react';
import { BrowserRouter as Router, Route, withRouter, Switch } from 'react-router-dom';
import styled, { ThemeProvider, createGlobalStyle } from 'styled-components';

import './css/indigo-static.css';
import './css/fonts.css';
import { light, dark, inverted, paperDark } from '@tlon/indigo-react';

import LaunchApp from './apps/launch/app';
import ChatApp from './apps/chat/app';
import DojoApp from './apps/dojo/app';
import GroupsApp from './apps/groups/app';
import LinksApp from './apps/links/app';
import PublishApp from './apps/publish/app';

import StatusBar from './components/StatusBar';
import NotFound from './components/404';

import GlobalStore from './store/store';
import GlobalSubscription from './subscription/global';
import GlobalApi from './api/global';

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
  }

  componentDidMount() {
    this.subscription.start();
    this.themeWatcher = window.matchMedia('(prefers-color-scheme: dark)');
    this.api.local.setDark(this.themeWatcher.matches);
    this.themeWatcher.addListener(this.updateTheme);
    this.api.local.getBaseHash();
  }

  componentWillUnmount() {
    this.themeWatcher.removeListener(this.updateTheme);
  }

  updateTheme(e) {
    this.api.local.setDark(e.matches);
  }

  render() {
    const channel = window.channel;

    const associations = this.state.associations ? this.state.associations : { contacts: {} };
    const selectedGroups = this.state.selectedGroups ? this.state.selectedGroups : [];
    const { state } = this;
    const theme = state.dark ? paperDark : light;

    return (
      <ThemeProvider theme={theme}>
        <Root>
          <Router>
            <StatusBarWithRouter props={this.props}
            associations={associations}
            invites={this.state.invites}
            api={this.api}
            connection={this.state.connection}
            subscription={this.subscription}
            />
            <Content>
            <Switch>
              <Route exact path="/"
              render={ p => (
                <LaunchApp
                  ship={this.ship}
                  api={this.api}
                  {...state}
                  {...p}
                />
              )}
              />
              <Route path="/~chat" render={ p => (
                <ChatApp
                  ship={this.ship}
                  api={this.api}
                  subscription={this.subscription}
                  {...state}
                  {...p}
                />
              )}
              />
              <Route path="/~dojo" render={ p => (
                <DojoApp
                  ship={this.ship}
                  channel={channel}
                  selectedGroups={selectedGroups}
                  subscription={this.subscription}
                  {...p}
                />
              )}
              />
              <Route path="/~groups" render={ p => (
                <GroupsApp
                  ship={this.ship}
                  api={this.api}
                  subscription={this.subscription}
                  {...state}
                  {...p}
                />
              )}
              />
              <Route path="/~link" render={ p => (
                <LinksApp
                  ship={this.ship}
                  ship={this.ship}
                  api={this.api}
                  subscription={this.subscription}
                  {...state}
                  {...p}
                />
              )}
              />
              <Route path="/~publish" render={ p => (
                <PublishApp
                  ship={this.ship}
                  api={this.api}
                  subscription={this.subscription}
                  {...state}
                  {...p}
                />
              )}
              />
              <Route component={NotFound} />
              </Switch>
            </Content>
          </Router>
        </Root>
      </ThemeProvider>
    );
  }
}

export default process.env.NODE_ENV === 'production' ? App : hot(App);

