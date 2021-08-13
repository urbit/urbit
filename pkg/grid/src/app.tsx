import React, { useEffect } from 'react';
import Mousetrap from 'mousetrap';
import { BrowserRouter, Switch, Route, useHistory } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from 'react-query';
import { Grid } from './pages/Grid';

export const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      refetchOnWindowFocus: false
    }
  }
});

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
    <QueryClientProvider client={queryClient}>
      <BrowserRouter basename={base}>
        <AppRoutes />
      </BrowserRouter>
    </QueryClientProvider>
  );
}
