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

import { Text, Anchor, Row } from '@tlon/indigo-react';

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
    scrollbar-color: ${ p => p.theme.colors.gray } transparent;
  }

  /* Works on Chrome/Edge/Safari */
  *::-webkit-scrollbar {
    width: 6px;
    height: 6px;
  }
  *::-webkit-scrollbar-track {
    background: transparent;
  }
  *::-webkit-scrollbar-thumb {
    background-color: ${ p => p.theme.colors.gray };
    border-radius: 1rem;
    border: 0px solid transparent;
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

    const notificationsCount = state.notificationsCount || 0;
    const doNotDisturb = state.doNotDisturb || false;

    const showBanner = localStorage.getItem("2020BreachBanner") || "flex";
    let banner = null;

    return (
      <ThemeProvider theme={theme}>
        <Helmet>
          {window.ship.length < 14
            ? <link rel="icon" type="image/svg+xml" href={this.faviconString()} />
            : null}
        </Helmet>
        <Root background={background}>
          <Row
            ref={e => banner = e}
            display={showBanner}
            justifyContent="space-between"
            width='100%'
            p='2'
            backgroundColor='yellow'>
          <Text color='#000000'>
            A network-wide breach is scheduled for early December 2020. Please visit <Anchor target="_blank" href="https://urbit.org/breach" color='inherit'>urbit.org/breach</Anchor> for more information.
              </Text>
            <Text cursor='pointer' fontWeight='500' onClick={() => {
              banner.style.display = "none";
              localStorage.setItem("2020BreachBanner", "none");
            }}>Dismiss</Text>
          </Row>
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
                doNotDisturb={doNotDisturb}
                notificationsCount={notificationsCount}
              />
            </ErrorBoundary>
            <ErrorBoundary>
              <Omnibox
                associations={state.associations}
                apps={state.launch}
                api={this.api}
                notifications={state.notificationsCount}
                invites={state.invites}
                groups={state.groups}
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

