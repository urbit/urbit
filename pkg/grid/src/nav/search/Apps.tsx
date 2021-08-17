import React, { useCallback, useEffect, useMemo } from 'react';
import { useQuery, useQueryClient } from 'react-query';
import { Link, RouteComponentProps } from 'react-router-dom';
import fuzzy from 'fuzzy';
import slugify from 'slugify';
import classNames from 'classnames';
import { ShipName } from '../../components/ShipName';
import { fetchProviderTreaties, treatyKey } from '../../state/docket';
import { Treaty } from '../../state/docket-types';
import { useLeapStore } from '../Nav';

type AppsProps = RouteComponentProps<{ ship: string }>;

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
        matches: results.map((treaty) => ({ value: treaty.desk, display: treaty.title }))
      });
    }
  }, [results]);

  const preloadApp = useCallback(
    (app: Treaty) => {
      queryClient.setQueryData(treatyKey([provider, app.desk]), app);
    },
    [queryClient]
  );

  const isSelected = useCallback(
    (target: Treaty) => {
      if (!selectedMatch) {
        return false;
      }

      const matchValue = selectedMatch.display || selectedMatch.value;
      return target.title === matchValue || target.desk === matchValue;
    },
    [selectedMatch]
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
        <ul className="space-y-8" aria-labelledby="developed-by">
          {results.map((app) => (
            <li
              key={app.desk}
              id={app.title || app.desk}
              role="option"
              aria-selected={isSelected(app)}
            >
              <Link
                to={`${match?.path.replace(':ship', provider)}/${slugify(app.desk)}`}
                className={classNames(
                  'flex items-center space-x-3 default-ring ring-offset-2 rounded-lg',
                  isSelected(app) && 'ring-4'
                )}
                onClick={() => preloadApp(app)}
              >
                <div
                  className="flex-none relative w-12 h-12 bg-gray-200 rounded-lg"
                  style={{ backgroundColor: app.color }}
                >
                  {app.img && (
                    <img
                      className="absolute top-1/2 left-1/2 h-[40%] w-[40%] object-contain transform -translate-x-1/2 -translate-y-1/2"
                      src={app.img}
                      alt=""
                    />
                  )}
                </div>
                <div className="flex-1 text-black">
                  <p>{app.title}</p>
                  {app.info && <p className="font-normal">{app.info}</p>}
                </div>
              </Link>
            </li>
          ))}
        </ul>
      )}
      <p>That&apos;s it!</p>
    </div>
  );
};
