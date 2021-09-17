import React, { useCallback, useEffect } from 'react';
import Mousetrap from 'mousetrap';
import { BrowserRouter, Switch, Route, useHistory, useLocation } from 'react-router-dom';
import { Grid } from './pages/Grid';
import useDocketState from './state/docket';
import { PermalinkRoutes } from './pages/PermalinkRoutes';
import useKilnState from './state/kiln';
import useContactState from './state/contact';
import api from './state/api';
import { useHarkStore } from './state/hark';
import { useTheme } from './state/settings';
import { useLocalState } from './state/local';

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

  useEffect(() => {
    const query = new URLSearchParams(location.search);
    if (query.has('grid-note')) {
      const redir = getNoteRedirect(query.get('grid-note')!);
      push(redir);
    }
  }, [location.search]);
  const theme = useTheme();

  const updateThemeClass = useCallback(
    (e: MediaQueryListEvent) => {
      if ((e.matches && theme === 'auto') || theme === 'dark') {
        document.body.classList.add('dark');
        useLocalState.setState({ currentTheme: 'dark' });
      } else {
        document.body.classList.remove('dark');
        useLocalState.setState({ currentTheme: 'light' });
      }
    },
    [theme]
  );

  useEffect(() => {
    const query = window.matchMedia('(prefers-color-scheme: dark)');

    query.addEventListener('change', updateThemeClass);
    updateThemeClass({ matches: query.matches } as MediaQueryListEvent);
    return () => {
      query.removeEventListener('change', updateThemeClass);
    };
  }, []);

  useEffect(() => {}, []);

  useEffect(() => {
    window.name = 'grid';

    const { fetchAllies, fetchCharges } = useDocketState.getState();
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
  }, []);

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
    <BrowserRouter basename={base}>
      <AppRoutes />
    </BrowserRouter>
  );
}
