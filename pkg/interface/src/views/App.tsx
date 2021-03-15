import { hot } from 'react-hot-loader/root';
import 'react-hot-loader';
import React, { useState, useEffect, useCallback } from 'react';
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

import { Content } from './landscape/components/Content';
import StatusBar from './components/StatusBar';
import Omnibox from './components/leap/Omnibox';
import ErrorBoundary from '~/views/components/ErrorBoundary';
import { TutorialModal } from '~/views/landscape/components/TutorialModal';

import { uxToHex } from '~/logic/lib/util';
import { foregroundFromBackground } from '~/logic/lib/sigil';
import withState from '~/logic/lib/withState';
import useLocalState from '~/logic/state/local';
import useContactState from '~/logic/state/contact';
import useSettingsState from '~/logic/state/settings';
import gcpManager from '~/logic/lib/gcpManager';
import useApi from '~/logic/api';

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

const App = (props) => {

  const [theme, setTheme] = useState(light);
  
  useEffect(() => {
    const themeWatcher = window.matchMedia('(prefers-color-scheme: dark)');
    themeWatcher.onchange = (event) => {
      setTheme(event.matches ? dark : light);
    };
    setTimeout(() => {
      themeWatcher.onchange ? themeWatcher.onchange(themeWatcher as any) : null;
    }, 500);
    Mousetrap.bindGlobal(['command+/', 'ctrl+/'], (event) => {
      event.preventDefault();
      event.stopImmediatePropagation();
      props.toggleOmnibox(); // TODO should be global state
    });

    return () => {
      themeWatcher.onchange = null;
    };
  });

  const contacts = useContactState(state => state.contacts);

  const faviconString = useCallback(() => {
    let background = '#ffffff';
    if (contacts.hasOwnProperty('/~/default')) {
      background = `#${uxToHex(contacts['/~/default'][window.ship].color)}`;
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
  }, [contacts]);

  useApi();

  return (
    <ThemeProvider theme={theme}>
      <Helmet>
        {window.ship.length < 14
          ? <link rel="icon" type="image/svg+xml" href={faviconString()} />
          : null}
      </Helmet>
      <Root>
        <Router>
          <TutorialModal />
          <ErrorBoundary>
            <StatusBarWithRouter />
          </ErrorBoundary>
          <ErrorBoundary>
            <Omnibox />
          </ErrorBoundary>
          <ErrorBoundary>
            <Content />
          </ErrorBoundary>
        </Router>
      </Root>
      <div id="portal-root" />
    </ThemeProvider>
  );
};

export default process.env.NODE_ENV === 'production' ? App : hot(App);
