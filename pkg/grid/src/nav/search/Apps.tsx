import React, { useCallback, useEffect, useMemo } from 'react';
import { useQuery, useQueryClient } from 'react-query';
import { RouteComponentProps } from 'react-router-dom';
import fuzzy from 'fuzzy';
import slugify from 'slugify';
import { ShipName } from '../../components/ShipName';
import { fetchProviderTreaties, treatyKey } from '../../state/docket';
import { Docket } from '../../state/docket-types';
import { MatchItem, useLeapStore } from '../Nav';
import { AppList } from '../../components/AppList';

type AppsProps = RouteComponentProps<{ ship: string }>;

export function appMatch(app: Docket): MatchItem {
  return { value: app.base, display: app.title };
}

export const Apps = ({ match }: AppsProps) => {
  const queryClient = useQueryClient();
  const { searchInput, selectedMatch, select } = useLeapStore((state) => ({
    searchInput: state.searchInput,
    select: state.select,
    selectedMatch: state.selectedMatch
  }));
  const provider = match?.params.ship;
  const { data: apps } = useQuery(treatyKey([provider]), () => fetchProviderTreaties(provider), {
    enabled: !!provider
  });
  const results = useMemo(
    () =>
      apps
        ? fuzzy
            .filter(
              searchInput,
              apps.map((t) => t.title)
            )
            .sort((a, b) => {
              const left = a.string.startsWith(searchInput) ? a.score + 1 : a.score;
              const right = b.string.startsWith(searchInput) ? b.score + 1 : b.score;

              return right - left;
            })
            .map((result) => apps[result.index])
        : undefined,
    [apps, searchInput]
  );
  const count = results?.length;

  useEffect(() => {
    select(
      <>
        Apps by <ShipName name={provider} className="font-mono" />
      </>
    );
  }, []);

  useEffect(() => {
    if (results) {
      useLeapStore.setState({
        matches: results.map(appMatch)
      });
    }
  }, [results]);

  const preloadApp = useCallback(
    (app: Docket) => {
      queryClient.setQueryData(treatyKey([provider, app.base]), app);
    },
    [queryClient]
  );

  return (
    <div className="dialog-inner-container md:px-6 md:py-8 h4 text-gray-400">
      <div id="developed-by">
        <h2 className="mb-3">
          Software developed by <ShipName name={provider} className="font-mono" />
        </h2>
        <p>
          {count} result{count === 1 ? '' : 's'}
        </p>
      </div>
      {results && (
        <AppList
          apps={results}
          labelledBy="developed-by"
          matchAgainst={selectedMatch}
          to={(app) => `${match?.path.replace(':ship', provider)}/${slugify(app.base)}`}
          onClick={(e, app) => preloadApp(app)}
        />
      )}
      <p>That&apos;s it!</p>
    </div>
  );
};
