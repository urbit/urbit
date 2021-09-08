import React, { useEffect } from 'react';
import Mousetrap from 'mousetrap';
import { BrowserRouter, Switch, Route, useHistory } from 'react-router-dom';
import { Grid } from './pages/Grid';
import useDocketState from './state/docket';
import {PermalinkRoutes} from './pages/PermalinkRoutes';
import useKilnState from './state/kiln';

const AppRoutes = () => {
  const { push } = useHistory();

  useEffect(() => {
    window.name = 'grid';

    const { fetchAllies, fetchCharges } = useDocketState.getState();
    fetchCharges();
    fetchAllies();
    const { fetchVats, fetchLag } = useKilnState.getState();
    fetchVats();
    fetchLag();
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
