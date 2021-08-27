import React, { useEffect, useMemo } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import fuzzy from 'fuzzy';
import { Provider } from '@urbit/api';
import { MatchItem, useLeapStore } from '../Nav';
import { useAllies } from '../../state/docket';
import { ProviderList } from '../../components/ProviderList';

type ProvidersProps = RouteComponentProps<{ ship: string }>;

export function providerMatch(provider: Provider | string): MatchItem {
  if (typeof provider === 'string') {
    return { value: provider, display: provider };
  }

  return { value: provider.shipName, display: provider.nickname };
}

export const Providers = ({ match }: ProvidersProps) => {
  const selectedMatch = useLeapStore((state) => state.selectedMatch);
  const provider = match?.params.ship;
  const allies = useAllies();
  const search = provider || '';
  const results = useMemo(
    () =>
      allies
        ? fuzzy
            .filter(
              search,
              Object.entries(allies).map(([ship]) => ship)
            )
            .sort((a, b) => {
              const left = a.string.startsWith(search) ? a.score + 1 : a.score;
              const right = b.string.startsWith(search) ? b.score + 1 : b.score;

              return right - left;
            })
            .map((el) => ({ shipName: el.original }))
        : [],
    [allies, search]
  );
  const count = results?.length;

  useEffect(() => {
    if (results) {
      useLeapStore.setState({
        matches: results.map(providerMatch)
      });
    }
  }, [results]);

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
