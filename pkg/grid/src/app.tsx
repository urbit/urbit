import React, { useEffect } from 'react';
import Mousetrap from 'mousetrap';
import { BrowserRouter, Switch, Route, useHistory } from 'react-router-dom';
import { Grid } from './pages/Grid';

const AppRoutes = () => {
  const { push } = useHistory();

  useEffect(() => {
    window.name = 'grid';

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
