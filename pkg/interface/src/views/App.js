import { hot } from 'react-hot-loader/root';
import 'react-hot-loader';
import * as React from 'react';
import { BrowserRouter as Router, withRouter } from 'react-router-dom';
import styled, { ThemeProvider, createGlobalStyle } from 'styled-components';
import { sigil as sigiljs, stringRenderer } from '@tlon/sigil-js';
import Helmet from 'react-helmet';

import Mousetrap from 'mousetrap';
import 'mousetrap-global-bind';

import './css/indigo-static.css';
import './css/fonts.css';
import light from './themes/light';
import dark from './themes/old-dark';

import { Content } from './landscape/components/Content';
import StatusBar from './components/StatusBar';
import Omnibox from './components/leap/Omnibox';
import ErrorBoundary from '~/views/components/ErrorBoundary';

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
  ${p => p.background?.type === 'url' ? `
    background-image: url('${p.background?.url}');
    background-size: cover;
    ` : p.background?.type === 'color' ? `
    background-color: ${p.background.color};
    ` : ''
  }
  display: flex;
  flex-flow: column nowrap;

  * {
    scrollbar-width: thin;
    scrollbar-color: ${ p => p.theme.colors.gray } ${ p => p.theme.colors.white };
  }

  /* Works on Chrome/Edge/Safari */
  *::-webkit-scrollbar {
    width: 12px;
  }
  *::-webkit-scrollbar-track {
    background: transparent;
  }
  *::-webkit-scrollbar-thumb {
    background-color: ${ p => p.theme.colors.gray };
    border-radius: 1rem;
    border: 3px solid ${ p => p.theme.colors.white };
  }
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
    this.faviconString = this.faviconString.bind(this);
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
      e.stopImmediatePropagation();
      this.api.local.setOmnibox();
    });
  }

  componentWillUnmount() {
    this.themeWatcher.removeListener(this.updateTheme);
  }

  updateTheme(e) {
    this.api.local.setDark(e.matches);
  }

  faviconString() {
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
    return dataurl;
  }

  render() {
    const { state } = this;
    const associations = state.associations ?
      state.associations : { contacts: {} };
    const theme = state.dark ? dark : light;
    const { background } = state;

    return (
      <ThemeProvider theme={theme}>
        <Helmet>
          {window.ship.length < 14
            ? <link rel="icon" type="image/svg+xml" href={this.faviconString()} />
            : null}
        </Helmet>
        <Root background={background}>
          <Router>
            <ErrorBoundary>
              <StatusBarWithRouter
                props={this.props}
                associations={associations}
                invites={this.state.invites}
                api={this.api}
                connection={this.state.connection}
                subscription={this.subscription}
                ship={this.ship}
              />
            </ErrorBoundary>
            <ErrorBoundary>
              <Omnibox
                associations={state.associations}
                apps={state.launch}
                api={this.api}
                dark={state.dark}
                show={state.omniboxShown}
              />
            </ErrorBoundary>
            <ErrorBoundary>
              <Content
                ship={this.ship}
                api={this.api}
                subscription={this.subscription}
                {...state}
              />
            </ErrorBoundary>
          </Router>
        </Root>
        <div id="portal-root" />
      </ThemeProvider>
    );
  }
}

export default process.env.NODE_ENV === 'production' ? App : hot(App);

