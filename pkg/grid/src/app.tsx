import React, { useEffect } from 'react';
import Mousetrap from 'mousetrap';
import { BrowserRouter, Switch, Route, useHistory, useLocation } from 'react-router-dom';
import { ErrorBoundary } from 'react-error-boundary';
import { Grid } from './pages/Grid';
import useDocketState from './state/docket';
import { PermalinkRoutes } from './pages/PermalinkRoutes';
import useKilnState from './state/kiln';
import useContactState from './state/contact';
import api from './state/api';
import { useMedia } from './logic/useMedia';
import { useHarkStore } from './state/hark';
import { useTheme } from './state/settings';
import { useLocalState } from './state/local';
import { ErrorAlert } from './components/ErrorAlert';
import { useErrorHandler } from './logic/useErrorHandler';

const getNoteRedirect = (path: string) => {
  if (path.startsWith('/desk/')) {
    const [, , desk] = path.split('/');
    return `/app/${desk}`;
  }
  return '';
};

const AppRoutes = () => {
  const { push } = useHistory();
  const { search } = useLocation();
  const handleError = useErrorHandler();

  useEffect(() => {
    const query = new URLSearchParams(search);
    if (query.has('grid-note')) {
      const redir = getNoteRedirect(query.get('grid-note')!);
      push(redir);
    }
  }, [search]);

  const theme = useTheme();
  const isDarkMode = useMedia('(prefers-color-scheme: dark)');

  useEffect(() => {
    if ((isDarkMode && theme === 'auto') || theme === 'dark') {
      document.body.classList.add('dark');
      useLocalState.setState({ currentTheme: 'dark' });
    } else {
      document.body.classList.remove('dark');
      useLocalState.setState({ currentTheme: 'light' });
    }
  }, [isDarkMode, theme]);

  useEffect(() => {}, []);

  useEffect(
    handleError(() => {
      window.name = 'grid';

      const { fetchDefaultAlly, fetchAllies, fetchCharges } = useDocketState.getState();
      fetchDefaultAlly();
      fetchCharges();
      fetchAllies();

      const { fetchVats, fetchLag } = useKilnState.getState();
      fetchVats();
      fetchLag();

      useContactState.getState().initialize(api);
      useHarkStore.getState().initialize(api);

      Mousetrap.bind(['command+/', 'ctrl+/'], () => {
        push('/leap/search');
      });
    }),
    []
  );

  return (
    <Switch>
      <Route path="/perma" component={PermalinkRoutes} />
      <Route path={['/leap/:menu', '/']} component={Grid} />
    </Switch>
  );
};

export function App() {
  const base = import.meta.env.MODE === 'mock' ? undefined : '/apps/grid';

  return (
    <ErrorBoundary FallbackComponent={ErrorAlert} onReset={() => window.location.reload()}>
      <BrowserRouter basename={base}>
        <AppRoutes />
      </BrowserRouter>
    </ErrorBoundary>
  );
}
