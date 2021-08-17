import fuzzy from 'fuzzy';
import classNames from 'classnames';
import React, { useCallback, useEffect, useMemo } from 'react';
import { useQuery } from 'react-query';
import { Link, RouteComponentProps } from 'react-router-dom';
import { ShipName } from '../../components/ShipName';
import { fetchProviders, providersKey } from '../../state/docket';
import { Provider } from '../../state/docket-types';
import { useLeapStore } from '../Nav';

type ProvidersProps = RouteComponentProps<{ ship: string }>;

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
        matches: results.map((p) => ({ value: p.shipName, display: p.nickname }))
      });
    }
  }, [JSON.stringify(results)]);

  const isSelected = useCallback(
    (target: Provider) => {
      if (!selectedMatch) {
        return false;
      }

      const matchValue = selectedMatch.display || selectedMatch.value;
      return target.nickname === matchValue || target.shipName === matchValue;
    },
    [selectedMatch]
  );

  return (
    <div className="dialog-inner-container md:px-6 md:py-8 h4 text-gray-400" aria-live="polite">
      <div id="providers">
        <h2 className="mb-3">Searching Software Providers</h2>
        <p>
          {count} result{count === 1 ? '' : 's'}
        </p>
      </div>
      {results && (
        <ul className="space-y-8" aria-labelledby="providers">
          {results.map((p) => (
            <li
              key={p.shipName}
              id={p.nickname || p.shipName}
              role="option"
              aria-selected={isSelected(p)}
            >
              <Link
                to={`${match?.path.replace(':ship', p.shipName)}/apps`}
                className={classNames(
                  'flex items-center space-x-3 default-ring ring-offset-2 rounded-lg',
                  isSelected(p) && 'ring-4'
                )}
              >
                <div className="flex-none relative w-12 h-12 bg-black rounded-lg">
                  {/* TODO: Handle sigils */}
                </div>
                <div className="flex-1 text-black">
                  <p className="font-mono">{p.nickname || <ShipName name={p.shipName} />}</p>
                  {p.status && <p className="font-normal">{p.status}</p>}
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
