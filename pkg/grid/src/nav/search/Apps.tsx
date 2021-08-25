import React, { useEffect, useMemo } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import fuzzy from 'fuzzy';
import slugify from 'slugify';
import { Docket } from '@urbit/api/docket';
import { ShipName } from '../../components/ShipName';
import useDocketState, { useAllyTreaties } from '../../state/docket';
import { MatchItem, useLeapStore } from '../Nav';
import { AppList } from '../../components/AppList';

type AppsProps = RouteComponentProps<{ ship: string }>;

export function appMatch(app: Docket): MatchItem {
  // TODO: do we need display vs value here,
  // will all apps have unique titles? If not,
  // what would we use?
  return { value: app.title, display: app.title };
}

export const Apps = ({ match }: AppsProps) => {
  const { searchInput, selectedMatch, select } = useLeapStore((state) => ({
    searchInput: state.searchInput,
    select: state.select,
    selectedMatch: state.selectedMatch
  }));
  const provider = match?.params.ship;
  const treaties = useAllyTreaties(provider);
  console.log(treaties);
  const results = useMemo(() => {
    if (!treaties) {
      return undefined;
    }
    const values = Object.values(treaties);
    return fuzzy
      .filter(
        searchInput,
        values.map((t) => t.title)
      )
      .sort((a, b) => {
        const left = a.string.startsWith(searchInput) ? a.score + 1 : a.score;
        const right = b.string.startsWith(searchInput) ? b.score + 1 : b.score;

        return right - left;
      })
      .map((result) => values[result.index]);
  }, [treaties, searchInput]);
  const count = results?.length;

  useEffect(() => {
    const { fetchAllyTreaties } = useDocketState.getState();
    fetchAllyTreaties(provider);
    select(
      <>
        Apps by <ShipName name={provider} className="font-mono" />
      </>
    );
  }, [provider]);

  useEffect(() => {
    if (results) {
      useLeapStore.setState({
        matches: results.map(appMatch)
      });
    }
  }, [results]);

  useEffect(() => {
    if (provider) {
      useDocketState.getState().fetchAllyTreaties(provider);
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
          to={(app) => `${match?.path.replace(':ship', provider)}/${app.ship}/${slugify(app.desk)}`}
        />
      )}
      <p>That&apos;s it!</p>
    </div>
  );
};
