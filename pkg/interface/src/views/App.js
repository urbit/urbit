import dark from '@tlon/indigo-dark';
import light from '@tlon/indigo-light';
import Mousetrap from 'mousetrap';
import 'mousetrap-global-bind';
import * as React from 'react';
import Helmet from 'react-helmet';
import 'react-hot-loader';
import { hot } from 'react-hot-loader/root';
import { BrowserRouter as Router, withRouter } from 'react-router-dom';
import styled, { ThemeProvider } from 'styled-components';
import gcpManager from '~/logic/lib/gcpManager';
import { favicon, svgDataURL } from '~/logic/lib/util';
import withState from '~/logic/lib/withState';
import useContactState from '~/logic/state/contact';
import useGroupState from '~/logic/state/group';
import useLocalState from '~/logic/state/local';
import useSettingsState from '~/logic/state/settings';
import useGraphState from '~/logic/state/graph';
import { ShortcutContextProvider } from '~/logic/lib/shortcutContext';

import ErrorBoundary from '~/views/components/ErrorBoundary';
import { TutorialModal } from '~/views/landscape/components/TutorialModal';
import './apps/chat/css/custom.css';
import Omnibox from './components/leap/Omnibox';
import StatusBar from './components/StatusBar';
import './css/fonts.css';
import './css/indigo-static.css';
import { Content } from './landscape/components/Content';
import './landscape/css/custom.css';
import { bootstrapApi } from '~/logic/api';
import useLaunchState from '../logic/state/launch';

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
  touch-action: none;

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

    //  eslint-disable-next-line

    this.updateTheme = this.updateTheme.bind(this);
    this.updateMobile = this.updateMobile.bind(this);
  }

  componentDidMount() {
    bootstrapApi();
    this.props.getShallowChildren(`~${window.ship}`, 'dm-inbox');
    const theme = this.getTheme();
    this.themeWatcher = window.matchMedia('(prefers-color-scheme: dark)');
    this.mobileWatcher = window.matchMedia(`(max-width: ${theme.breakpoints[0]})`);
    this.themeWatcher.onchange = this.updateTheme;
    this.mobileWatcher.onchange = this.updateMobile;
    setTimeout(() => {
      // Something about how the store works doesn't like changing it
      // before the app has actually rendered, hence the timeout.
      this.updateMobile(this.mobileWatcher);
      this.updateTheme(this.themeWatcher);
    }, 500);
    this.props.getBaseHash();
    this.props.getRuntimeLag();  // TODO  consider polling periodically
    this.props.getAll();
    gcpManager.start();
    Mousetrap.bindGlobal(['command+/', 'ctrl+/'], (e) => {
      e.preventDefault();
      e.stopImmediatePropagation();
      this.props.toggleOmnibox();
    });
  }

  componentWillUnmount() {
    this.themeWatcher.onchange = undefined;
    this.mobileWatcher.onchange = undefined;
  }

  updateTheme(e) {
    this.props.set((state) => {
      state.dark = e.matches;
    });
  }

  updateMobile(e) {
    this.props.set((state) => {
      state.mobile = e.matches;
    });
  }

  getTheme() {
    const { props } = this;
    return ((props.dark && props?.display?.theme == 'auto') ||
      props?.display?.theme == 'dark'
    ) ? dark : light;
  }

  render() {
    const theme = this.getTheme();

    const ourContact = this.props.contacts[`~${this.ship}`] || null;
    return (
      <ThemeProvider theme={theme}>
        <ShortcutContextProvider>
        <Helmet>
          {window.ship.length < 14
            ? <link rel="icon" type="image/svg+xml" href={svgDataURL(favicon())} />
            : null}
        </Helmet>
        <Root>
          <Router>
            <TutorialModal />
            <ErrorBoundary>
              <StatusBarWithRouter
                props={this.props}
                ourContact={ourContact}
                connection={'foo'}
                subscription={this.subscription}
                ship={this.ship}
              />
            </ErrorBoundary>
            <ErrorBoundary>
              <Omnibox
                show={this.props.omniboxShown}
                toggle={this.props.toggleOmnibox}
              />
            </ErrorBoundary>
            <ErrorBoundary>
              <Content
                ship={this.ship}
                subscription={this.subscription}
                connection={'aa'}
              />
            </ErrorBoundary>
          </Router>
        </Root>
        <div id="portal-root" />
        </ShortcutContextProvider>
      </ThemeProvider>
    );
  }
}

export default withState(process.env.NODE_ENV === 'production' ? App : hot(App), [
  [useGroupState],
  [useContactState],
  [useSettingsState, ['display', 'getAll']],
  [useLocalState],
  [useGraphState, ['getShallowChildren']],
  [useLaunchState, ['getRuntimeLag', 'getBaseHash']]
]);
