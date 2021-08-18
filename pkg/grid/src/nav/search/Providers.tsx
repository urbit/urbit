import fuzzy from 'fuzzy';
import React, { useEffect, useMemo } from 'react';
import { useQuery } from 'react-query';
import { RouteComponentProps } from 'react-router-dom';
import { fetchProviders, providersKey } from '../../state/docket';
import { Provider } from '../../state/docket-types';
import { MatchItem, useLeapStore } from '../Nav';
import { ProviderList } from '../../components/ProviderList';

type ProvidersProps = RouteComponentProps<{ ship: string }>;

export function providerMatch(provider: Provider): MatchItem {
  return { value: provider.shipName, display: provider.nickname };
}

export const Providers = ({ match }: ProvidersProps) => {
  const { selectedMatch, select } = useLeapStore((state) => ({
    select: state.select,
    selectedMatch: state.selectedMatch
  }));
  const provider = match?.params.ship;
  const { data: providers } = useQuery(providersKey(), () => fetchProviders(), {
    enabled: !!provider,
    keepPreviousData: true
  });
  const search = provider || '';
  const results = useMemo(
    () =>
      providers
        ? fuzzy
            .filter(
              search,
              providers.map((p) => p.shipName + (p.nickname || ''))
            )
            .sort((a, b) => {
              const left = a.string.startsWith(search) ? a.score + 1 : a.score;
              const right = b.string.startsWith(search) ? b.score + 1 : b.score;

              return right - left;
            })
            .map((el) => providers[el.index])
        : [],
    [providers, search]
  );
  const count = results?.length;

  useEffect(() => {
    select(null, provider);
  }, []);

  useEffect(() => {
    if (results) {
      useLeapStore.setState({
        matches: results.map(providerMatch)
      });
    }
  }, [JSON.stringify(results)]);

  return (
    <div className="dialog-inner-container md:px-6 md:py-8 h4 text-gray-400" aria-live="polite">
      <div id="providers">
        <h2 className="mb-3">Searching Software Providers</h2>
        <p>
          {count} result{count === 1 ? '' : 's'}
        </p>
      </div>
      {results && (
        <ProviderList providers={results} labelledBy="providers" matchAgainst={selectedMatch} />
      )}
      <p>That&apos;s it!</p>
    </div>
  );
};
