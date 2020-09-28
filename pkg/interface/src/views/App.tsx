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
import { uxToHex, blobUrl } from '~/logic/lib/util';
import { foregroundFromBackground } from '~/logic/lib/sigil';
import Manifest, { PWAManifest } from '~/views/components/Manifest';
import { StoreState } from '~/logic/store/type';

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

type AppState = StoreState & {
  manifest: PWAManifest;
}

class App extends React.Component<{}, AppState> {
  private ship: string;
  private store: GlobalStore;
  private appChannel: any; // Comes from channel.js
  private api: GlobalApi;
  private subscription: GlobalSubscription;
  private themeWatcher?: MediaQueryList;

  constructor(props) {
    super(props);
    this.ship = window.ship;
    this.store = new GlobalStore();
    this.store.setStateHandler(this.setState.bind(this));
    
    this.appChannel = new window.channel();
    this.api = new GlobalApi(this.ship, this.appChannel, this.store);
    this.subscription =
    new GlobalSubscription(this.store, this.api, this.appChannel);
    
    this.updateTheme = this.updateTheme.bind(this);
    this.icon = this.icon.bind(this);
    this.defaultManifest = this.defaultManifest.bind(this);
    this.state = this.store.state;
    this.state.manifest = this.defaultManifest();
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
    if (!this.themeWatcher) return;
    this.themeWatcher.removeListener(this.updateTheme);
  }

  defaultManifest(): PWAManifest {
    return {
      name: window.ship,
      short_name: 'OS1',
      background_color: '#ffffff',
      theme_color: '#000000',
      start_url: window.location.origin,
      display: 'fullscreen',
      icons: [{
        src: '/~landscape/img/os1-app.png',
        sizes: '512x512',
        type: 'image/png'
      }, {
        src: blobUrl(this.icon(), 'image/svg+xml'),
        type: 'image/svg+xml',
        sizes: '512x512'
      }],
      orientation: 'any',
      scope: '/'
    };
  }

  updateTheme(e): void {
    this.api.local.setDark(e.matches);
  }

  icon(): string {
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
    return svg;
  }

  render() {
    const { state } = this;
    const associations = state.associations ?
      state.associations : { contacts: {} };
    const theme = state.dark ? dark : light;
    const { background } = state;

    return (
      <ThemeProvider theme={theme}>
        <Manifest data={this.state.manifest} />
        <Helmet>
          {window.ship.length < 14
            ? <link rel="icon" type="image/svg+xml" href={blobUrl(this.icon(), 'image/svg+xml')} />
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

