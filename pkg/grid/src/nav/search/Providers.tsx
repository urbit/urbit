import classNames from 'classnames';
import React, { useCallback, useEffect, useMemo } from 'react';
import { Link, RouteComponentProps } from 'react-router-dom';
import fuzzy from 'fuzzy';
import { ShipName } from '../../components/ShipName';
import { useLeapStore } from '../Nav';
import { useAllies } from '../../state/docket';

type ProvidersProps = RouteComponentProps<{ ship: string }>;

export const Providers = ({ match }: ProvidersProps) => {
  const { selectedMatch, select } = useLeapStore((state) => ({
    select: state.select,
    selectedMatch: state.selectedMatch
  }));
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
            .map((el) => {
              console.log(el);
              return el.original;
            })
        : [],
    [allies, search]
  );
  const count = results?.length;
  const ally = match?.params.ship;

  useEffect(() => {
    select(null, ally);
  }, [ally]);

  useEffect(() => {
    if (results) {
      useLeapStore.setState({
        matches: results.map((p) => ({ value: p, display: p }))
      });
    }
  }, [results]);

  const isSelected = useCallback(
    (target: string) => {
      if (!selectedMatch) {
        return false;
      }

      const matchValue = selectedMatch.display || selectedMatch.value;
      return target === matchValue;
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
            <li key={p} id={p} role="option" aria-selected={isSelected(p)}>
              <Link
                to={`${match?.path.replace(':ship', p)}/apps`}
                className={classNames(
                  'flex items-center space-x-3 default-ring ring-offset-2 rounded-lg',
                  isSelected(p) && 'ring-4'
                )}
              >
                <div className="flex-none relative w-12 h-12 bg-black rounded-lg">
                  {/* TODO: Handle sigils */}
                </div>
                <div className="flex-1 text-black">
                  <p className="font-mono">
                    <ShipName name={p} />
                  </p>
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
