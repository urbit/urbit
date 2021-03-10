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
import light from '@tlon/indigo-light';
import dark from '@tlon/indigo-dark';

import { Text, Anchor, Row } from '@tlon/indigo-react';

import { Content } from './landscape/components/Content';
import StatusBar from './components/StatusBar';
import Omnibox from './components/leap/Omnibox';
import ErrorBoundary from '~/views/components/ErrorBoundary';
import { TutorialModal } from '~/views/landscape/components/TutorialModal';

import GlobalStore from '~/logic/store/store';
import GlobalSubscription from '~/logic/subscription/global';
import GlobalApi from '~/logic/api/global';
import { uxToHex } from '~/logic/lib/util';
import { foregroundFromBackground } from '~/logic/lib/sigil';
import withState from '~/logic/lib/withState';
import useLocalState from '~/logic/state/local';
import useContactState from '~/logic/state/contact';
import useGroupState from '~/logic/state/group';
import useSettingsState from '~/logic/state/settings';
import gcpManager from '~/logic/lib/gcpManager';


const Root = withState(styled.div`
  font-family: ${p => p.theme.fonts.sans};
  height: 100%;
  width: 100%;
  padding: 0;
  margin: 0;
  ${p => p.display.backgroundType === 'url' ? `
    background-image: url('${p.display.background}');
    background-size: cover;
    ` : p.display.backgroundType === 'color' ? `
    background-color: ${p.display.background};
    ` : `background-color: ${p.theme.colors.white};`
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
`, [
  [useSettingsState, ['display']]
]);

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
    gcpManager.configure(this.api);
    this.subscription =
      new GlobalSubscription(this.store, this.api, this.appChannel);

    this.updateTheme = this.updateTheme.bind(this);
    this.faviconString = this.faviconString.bind(this);
  }

  componentDidMount() {
    this.subscription.start();
    this.themeWatcher = window.matchMedia('(prefers-color-scheme: dark)');
    this.themeWatcher.onchange = this.updateTheme;
    setTimeout(() => {
      // Something about how the store works doesn't like changing it
      // before the app has actually rendered, hence the timeout.
      this.updateTheme(this.themeWatcher);
    }, 500);
    this.api.local.getBaseHash();
    this.api.settings.getAll();
    gcpManager.start();
    Mousetrap.bindGlobal(['command+/', 'ctrl+/'], (e) => {
      e.preventDefault();
      e.stopImmediatePropagation();
      this.props.toggleOmnibox();
    });
  }

  componentWillUnmount() {
    this.themeWatcher.onchange = undefined;
  }

  updateTheme(e) {
    this.props.set(state => {
      state.dark = e.matches;
    });
  }

  faviconString() {
    let background = '#ffffff';
    if (this.props.contacts.hasOwnProperty(`~${window.ship}`)) {
      background = `#${uxToHex(this.props.contacts[`~${window.ship}`].color)}`;
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
    const { state, props } = this;
    const theme =
    ((props.dark && props?.display?.theme == "auto") ||
      props?.display?.theme == "dark"
    ) ? dark : light;

    const ourContact = this.props.contacts[`~${this.ship}`] || null;
    return (
      <ThemeProvider theme={theme}>
        <Helmet>
          {window.ship.length < 14
            ? <link rel="icon" type="image/svg+xml" href={this.faviconString()} />
            : null}
        </Helmet>
        <Root>
          <Router>
            <TutorialModal api={this.api} />
            <ErrorBoundary>
              <StatusBarWithRouter
                props={this.props}
                ourContact={ourContact}
                api={this.api}
                connection={this.state.connection}
                subscription={this.subscription}
                ship={this.ship}
              />
            </ErrorBoundary>
            <ErrorBoundary>
              <Omnibox
                associations={state.associations}
                api={this.api}
                show={this.props.omniboxShown}
                toggle={this.props.toggleOmnibox}
              />
            </ErrorBoundary>
            <ErrorBoundary>
              <Content
                ship={this.ship}
                api={this.api}
                subscription={this.subscription}
              />
            </ErrorBoundary>
          </Router>
        </Root>
        <div id="portal-root" />
      </ThemeProvider>
    );
  }
}

export default withState(process.env.NODE_ENV === 'production' ? App : hot(App), [
  [useGroupState],
  [useContactState],
  [useSettingsState, ['display']],
  [useLocalState]
]);