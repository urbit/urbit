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

import { Content } from './components/Content';
import StatusBar from './components/StatusBar';
import Omnibox from './components/Omnibox';
import ErrorComponent from './components/Error';

import GlobalStore from '~/logic/store/store';
import GlobalSubscription from '~/logic/subscription/global';
import GlobalApi from '~/logic/api/global';
import { uxToHex } from '~/logic/lib/util';
import { foregroundFromBackground } from '~/logic/lib/sigil';

const Root = styled.div`
  font-family: ${p => p.theme.fonts.sans};
  height: 100%;
  width: 100%;
  padding: 0;
  margin: 0;
  display: flex;
  flex-flow: column nowrap;
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
    Mousetrap.bindGlobal(['command+/', 'ctrl+/'], (e) => {
      e.preventDefault();
      e.stopImmediatePropagation();
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
      const foreground = foregroundFromBackground(background);
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
    const { state } = this;
    const associations = state.associations ?
      state.associations : { contacts: {} };
    const theme = state.dark ? dark : light;

    return (
      <ThemeProvider theme={theme}>
        <Root>
          <Router>
            <StatusBarWithRouter
              props={this.props}
              associations={associations}
              invites={this.state.invites}
              api={this.api}
              connection={this.state.connection}
              subscription={this.subscription}
            />
            <Omnibox
              associations={state.associations}
              apps={state.launch}
              api={this.api}
              dark={state.dark}
              show={state.omniboxShown}
            />
          <Content
            ship={this.ship}
            api={this.api}
            subscription={this.subscription}
            {...state} />
          </Router>
        </Root>
      </ThemeProvider>
    );
  }
}


export default process.env.NODE_ENV === 'production' ? App : hot(App);

