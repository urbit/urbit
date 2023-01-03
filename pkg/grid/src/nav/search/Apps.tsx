import React, { useCallback, useEffect, useMemo } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import fuzzy from 'fuzzy';
import { Treaty } from '@urbit/api';
import { ShipName } from '../../components/ShipName';
import { useAllyTreaties } from '../../state/docket';
import { useLeapStore } from '../Nav';
import { AppList } from '../../components/AppList';
import { addRecentDev } from './Home';
import { Spinner } from '../../components/Spinner';

type AppsProps = RouteComponentProps<{ ship: string }>;

export const Apps = ({ match }: AppsProps) => {
  const { searchInput, selectedMatch, select } = useLeapStore((state) => ({
    searchInput: state.searchInput,
    select: state.select,
    selectedMatch: state.selectedMatch
  }));
  const provider = match?.params.ship;
  const { treaties, status } = useAllyTreaties(provider);

  useEffect(() => {
    if (provider) {
      addRecentDev(provider);
    }
  }, [provider]);

  const results = useMemo(() => {
    if (!treaties) {
      return undefined;
    }
    const values = Object.values(treaties);
    return fuzzy
      .filter(
        searchInput,
        values.map((v) => v.title)
      )
      .sort((a, b) => {
        const left = a.string.startsWith(searchInput) ? a.score + 1 : a.score;
        const right = b.string.startsWith(searchInput) ? b.score + 1 : b.score;

        return right - left;
      })
      .map((result) => values[result.index]);
  }, [treaties, searchInput]);
  const count = results?.length;

  const getAppPath = useCallback(
    (app: Treaty) => `${match?.path.replace(':ship', provider)}/${app.ship}/${app.desk}`,
    [match]
  );

  useEffect(() => {
    select(
      <>
        Apps by <ShipName name={provider} className="font-mono" />
      </>
    );
  }, [provider]);

  useEffect(() => {
    if (results) {
      useLeapStore.setState({
        matches: results.map((r) => ({
          url: getAppPath(r),
          openInNewTab: false,
          value: r.desk,
          display: r.title
        }))
      });
    }
  }, [results]);

  const showNone =
    status === 'error' || ((status === 'success' || status === 'initial') && results?.length === 0);

  return (
    <div className="dialog-inner-container md:px-6 md:py-8 h4 text-gray-400">
      {status === 'loading' && (
        <span className="mb-3">
          <Spinner className="w-7 h-7 mr-3" /> Finding software...
        </span>
      )}
      {results && results.length > 0 && (
        <>
          <div id="developed-by">
            <h2 className="mb-3">
              Software developed by <ShipName name={provider} className="font-mono" />
            </h2>
            <p>
              {count} result{count === 1 ? '' : 's'}
            </p>
          </div>
          <AppList
            apps={results}
            labelledBy="developed-by"
            matchAgainst={selectedMatch}
            to={getAppPath}
          />
          <p>That&apos;s it!</p>
        </>
      )}
      {showNone && (
        <h2>
          Unable to find software developed by <ShipName name={provider} className="font-mono" />
        </h2>
      )}
    </div>
  );
};
