import React, { useCallback, useEffect } from 'react';
import Mousetrap from 'mousetrap';
import { BrowserRouter, Switch, Route, useHistory } from 'react-router-dom';
import { Grid } from './pages/Grid';
import useDocketState from './state/docket';
import { usePreferencesStore } from './nav/preferences/usePreferencesStore';
import useContactState from './state/contact';
import api from './state/api';

const AppRoutes = () => {
  const { push } = useHistory();
  const theme = usePreferencesStore((s) => s.theme);

  const updateThemeClass = useCallback(
    (e: MediaQueryListEvent) => {
      if ((e.matches && theme === 'automatic') || theme === 'dark') {
        document.body.classList.add('dark');
        usePreferencesStore.setState({ currentTheme: 'dark' });
      } else {
        document.body.classList.remove('dark');
        usePreferencesStore.setState({ currentTheme: 'light' });
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

  useEffect(() => {
    window.name = 'grid';

    const { fetchAllies, fetchCharges } = useDocketState.getState();
    fetchCharges();
    fetchAllies();
    useContactState.getState().initialize(api);

    Mousetrap.bind(['command+/', 'ctrl+/'], () => {
      push('/leap/search');
    });
  }, []);

  return (
    <Switch>
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
