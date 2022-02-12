import dark from '@tlon/indigo-dark';
import light from '@tlon/indigo-light';
import Mousetrap from 'mousetrap';
import shallow from 'zustand/shallow';
import 'mousetrap-global-bind';
import * as React from 'react';
import Helmet from 'react-helmet';
import 'react-hot-loader';
import { hot } from 'react-hot-loader/root';
import { BrowserRouter as Router, withRouter } from 'react-router-dom';
import styled, { ThemeProvider } from 'styled-components';
import gcpManager from '~/logic/lib/gcpManager';
import { svgDataURL } from '~/logic/lib/util';
import withState from '~/logic/lib/withState';
import useContactState, { favicon } from '~/logic/state/contact';
import useLocalState from '~/logic/state/local';
import useSettingsState from '~/logic/state/settings';
import useGraphState from '~/logic/state/graph';
import { ShortcutContextProvider } from '~/logic/lib/shortcutContext';

import ErrorBoundary from '~/views/components/ErrorBoundary';
import './apps/chat/css/custom.css';
import Omnibox from './components/leap/Omnibox';
import StatusBar from './components/StatusBar';
import './css/fonts.css';
import './css/indigo-static.css';
import { Content } from './landscape/components/Content';
import './landscape/css/custom.css';
import { bootstrapApi } from '~/logic/api/bootstrap';
import { uxToHex } from '@urbit/api';

function ensureValidHex(color) {
  if (!color)
    return '#000000';

  const isUx = color.startsWith('0x');
  const parsedColor = isUx ? uxToHex(color) : color;

  return parsedColor.startsWith('#') ? parsedColor : `#${parsedColor}`;
}

const Root = withState(styled.div`
  font-family: ${p => p.theme.fonts.sans};
  height: 100%;
  width: 100%;
  padding-left: env(safe-area-inset-left, 0px);
  padding-right: env(safe-area-inset-right, 0px);
  padding-top: env(safe-area-inset-top, 0px);
  padding-bottom: env(safe-area-inset-bottom, 0px);
  
  margin: 0;
  ${p => p.display.backgroundType === 'url' ? `
    background-image: url('${p.display.background}');
    background-size: cover;
    ` : p.display.backgroundType === 'color' ? `
    background-color: ${ensureValidHex(p.display.background)};
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

    this.updateTheme = this.updateTheme.bind(this);
    this.updateMobile = this.updateMobile.bind(this);
  }

  componentDidMount() {
    bootstrapApi();
    this.props.getShallowChildren(`~${window.ship}`, 'dm-inbox');
    const theme = this.getTheme();
    setTimeout(() => {
      // Something about how the store works doesn't like changing it
      // before the app has actually rendered, hence the timeout.
      this.themeWatcher = window.matchMedia('(prefers-color-scheme: dark)');
      this.mobileWatcher = window.matchMedia(`(max-width: ${theme.breakpoints[0]})`);
      this.smallWatcher = window.matchMedia(`(min-width: ${theme.breakpoints[0]})`);
      this.mediumWatcher = window.matchMedia(`(min-width: ${theme.breakpoints[1]})`);
      this.largeWatcher = window.matchMedia(`(min-width: ${theme.breakpoints[2]})`);
      // TODO: addListener is deprecated, but safari 13 requires it
      this.themeWatcher.addListener(this.updateTheme);
      this.mobileWatcher.addListener(this.updateMobile);
      this.smallWatcher.addListener(this.updateSmall);
      this.mediumWatcher.addListener(this.updateMedium);
      this.largeWatcher.addListener(this.updateLarge);

      this.updateMobile(this.mobileWatcher);
      this.updateSmall(this.updateSmall);
      this.updateTheme(this.themeWatcher);
      this.updateMedium(this.mediumWatcher);
      this.updateLarge(this.largeWatcher);
    }, 500);
    this.props.getAll();
    gcpManager.start();
    Mousetrap.bindGlobal(['command+/', 'ctrl+/'], (e) => {
      e.preventDefault();
      e.stopImmediatePropagation();
      this.props.toggleOmnibox();
    });
  }

  componentWillUnmount() {
    this.themeWatcher.removeListener(this.updateTheme);
    this.mobileWatcher.removeListener(this.updateMobile);
    this.smallWatcher.removeListener(this.updateSmall);
    this.mediumWatcher.removeListener(this.updateMedium);
    this.largeWatcher.removeListener(this.updateLarge);
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

  updateSmall = (e) => {
    this.props.set((state) => {
      state.breaks.sm = e.matches;
    });
  }

  updateMedium = (e) => {
    this.props.set((state) => {
      state.breaks.md = e.matches;
    });
  }

  updateLarge = (e) => {
    this.props.set((state) => {
      state.breaks.lg = e.matches;
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

    const { ourContact } = this.props;
    return (
      <ThemeProvider theme={theme}>
        <ShortcutContextProvider>
          <Helmet>
            {window.ship.length < 14
              ? <link rel="icon" type="image/svg+xml" href={svgDataURL(favicon())} />
              : null}
          </Helmet>
          <Root>
            <Router basename="/apps/escape">
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
const WarmApp = process.env.NODE_ENV === 'production' ? App : hot(App);

const selContacts = s => s.contacts[`~${window.ship}`];
const selLocal = s => [s.set, s.omniboxShown, s.toggleOmnibox, s.dark];
const selSettings = s => [s.display, s.getAll];
const selGraph = s => s.getShallowChildren;

const WithApp = React.forwardRef((props, ref) => {
  const ourContact = useContactState(selContacts);
  const [display, getAll] = useSettingsState(selSettings, shallow);
  const [setLocal, omniboxShown, toggleOmnibox, dark] = useLocalState(selLocal);
  const getShallowChildren = useGraphState(selGraph);

  return (
    <WarmApp
      ref={ref}
      ourContact={ourContact}
      display={display}
      getAll={getAll}
      set={setLocal}
      dark={dark}
      getShallowChildren={getShallowChildren}
      toggleOmnibox={toggleOmnibox}
      omniboxShown={omniboxShown}
    />
  );
});

WarmApp.whyDidYouRender = true;

export default WithApp;

