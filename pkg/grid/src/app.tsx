import React, { useEffect } from 'react';
import Mousetrap from 'mousetrap';
import { BrowserRouter, Switch, Route, useHistory } from 'react-router-dom';
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

const AppRoutes = () => {
  const { push } = useHistory();
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
