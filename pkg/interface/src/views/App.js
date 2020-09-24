import { hot } from 'react-hot-loader/root';
import 'react-hot-loader';
import * as React from 'react';
import { BrowserRouter as Router, withRouter } from 'react-router-dom';
import styled, { ThemeProvider, createGlobalStyle } from 'styled-components';
import { sigil as sigiljs, stringRenderer } from 'urbit-sigil-js';
import Helmet from 'react-helmet';

import Mousetrap from 'mousetrap';
import 'mousetrap-global-bind';

import './css/indigo-static.css';
import './css/fonts.css';
import light from './themes/light';
import dark from './themes/old-dark';

import { Content } from './components/Content';
import StatusBar from './components/StatusBar';
import Omnibox from './components/leap/Omnibox';

import GlobalStore from '~/logic/store/store';
import GlobalSubscription from '~/logic/subscription/global';
import GlobalApi from '~/logic/api/global';
import { uxToHex, manifestUrl, cite, blobUrl } from '~/logic/lib/util';
import { foregroundFromBackground } from '~/logic/lib/sigil';
import Manifest from '~/views/components/Manifest';

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
    background: ${ p => p.theme.colors.white };
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
    this.icon = this.icon.bind(this);
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

  icon() {
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
  }

  faviconString() {
    const dataurl = 'data:image/svg+xml;base64,' + btoa(this.icon());
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
        <Manifest data={{
          name: window.ship,
          short_name: 'OS1',
          background_color: '#ffffff',
          theme_color: '#000000',
          start_url: window.location.origin,
          display: 'minimal-ui',
          icons: [{
            src: 'https://user-images.githubusercontent.com/1195363/93526624-b67cb400-f905-11ea-8888-7423c89d3878.png',
            sizes: '512x512',
            type: 'image/png'
          }, {
            src: blobUrl(this.icon(), 'image/svg+xml'),
            type: 'image/svg+xml',
            sizes: '512x512'
          }],
          orientation: 'any',
          scope: '/'
        }}/>
        <Helmet>
          {window.ship.length < 14
            ? <link rel="icon" type="image/svg+xml" href={this.faviconString()} />
            : null}
          <meta name="apple-mobile-web-app-status-bar-style" content="default" />
        </Helmet>
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
          <Content
            ship={this.ship}
            api={this.api}
            subscription={this.subscription}
            {...state}
          />
          </Router>
        </Root>
      </ThemeProvider>
    );
  }
}

export default process.env.NODE_ENV === 'production' ? App : hot(App);

