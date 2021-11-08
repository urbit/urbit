import React from 'react';
import { Route, RouteComponentProps, Switch } from 'react-router-dom';
import { ErrorBoundary } from 'react-error-boundary';
import { TreatyInfo } from './search/TreatyInfo';
import { Apps } from './search/Apps';
import { Home } from './search/Home';
import { Providers } from './search/Providers';
import { ErrorAlert } from '../components/ErrorAlert';

type SearchProps = RouteComponentProps<{
  query?: string;
}>;

export const Search = ({ match, history }: SearchProps) => {
  return (
    <ErrorBoundary FallbackComponent={ErrorAlert} onReset={() => history.push('/leap/search')}>
      <Switch>
        <Route
          path={[`${match.path}/direct/apps/:host/:desk`, `${match.path}/:ship/apps/:host/:desk`]}
          component={TreatyInfo}
        />
        <Route path={`${match.path}/:ship/apps`} component={Apps} />
        <Route path={`${match.path}/:ship`} component={Providers} />
        <Route path={`${match.path}`} component={Home} />
      </Switch>
    </ErrorBoundary>
  );
};
