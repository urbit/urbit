import React, { useEffect, useMemo, useState } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import fuzzy from 'fuzzy';
import slugify from 'slugify';
import { ShipName } from '../../components/ShipName';
import { Docket, Treaty } from '../../state/docket-types';
import { MatchItem, useLeapStore } from '../Nav';
import { AppList } from '../../components/AppList';
import useDocketState from '../../state/docket';

type AppsProps = RouteComponentProps<{ ship: string }>;

export function appMatch(app: Docket): MatchItem {
  return { value: app.base, display: app.title };
}

export const Apps = ({ match }: AppsProps) => {
  const { searchInput, selectedMatch, select } = useLeapStore((state) => ({
    searchInput: state.searchInput,
    select: state.select,
    selectedMatch: state.selectedMatch
  }));
  const provider = match?.params.ship;
  const fetchProviderTreaties = useDocketState((s) => s.fetchProviderTreaties);
  const [treaties, setTreaties] = useState<Treaty[]>();
  const results = useMemo(
    () =>
      treaties
        ? fuzzy
            .filter(
              searchInput,
              treaties.map((t) => t.title)
            )
            .sort((a, b) => {
              const left = a.string.startsWith(searchInput) ? a.score + 1 : a.score;
              const right = b.string.startsWith(searchInput) ? b.score + 1 : b.score;

              return right - left;
            })
            .map((result) => treaties[result.index])
        : undefined,
    [treaties, searchInput]
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

  useEffect(() => {
    async function getTreaties() {
      setTreaties(await fetchProviderTreaties(provider));
    }

    if (provider) {
      getTreaties();
    }
  }, [provider]);

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
        />
      )}
      <p>That&apos;s it!</p>
    </div>
  );
};
