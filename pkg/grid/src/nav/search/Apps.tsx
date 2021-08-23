import React, { useCallback, useEffect, useMemo } from 'react';
import { Link, RouteComponentProps } from 'react-router-dom';
import fuzzy from 'fuzzy';
import slugify from 'slugify';
import classNames from 'classnames';
import { Treaty } from '@urbit/api/docket';
import { ShipName } from '../../components/ShipName';
import useDocketState, { useAllyTreaties } from '../../state/docket';
import { useLeapStore } from '../Nav';

type AppsProps = RouteComponentProps<{ ship: string }>;

export const Apps = ({ match }: AppsProps) => {
  const { searchInput, selectedMatch, select } = useLeapStore((state) => ({
    searchInput: state.searchInput,
    select: state.select,
    selectedMatch: state.selectedMatch
  }));
  const provider = match?.params.ship;
  const treaties = useAllyTreaties(provider);
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

  useEffect(() => {
    if (provider) {
      useDocketState.getState().fetchAllyTreaties(provider);
    }
  }, [provider]);

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
          {results.map((treaty) => (
            <li
              key={treaty.desk}
              id={treaty.title || treaty.desk}
              role="option"
              aria-selected={isSelected(treaty)}
            >
              <Link
                to={`${match?.path.replace(':ship', provider)}/${treaty.ship}/${slugify(
                  treaty.desk
                )}`}
                className={classNames(
                  'flex items-center space-x-3 default-ring ring-offset-2 rounded-lg',
                  isSelected(treaty) && 'ring-4'
                )}
              >
                <div
                  className="flex-none relative w-12 h-12 bg-gray-200 rounded-lg"
                  style={{ backgroundColor: treaty.color }}
                >
                  {treaty.image && (
                    <img
                      className="absolute top-1/2 left-1/2 h-[40%] w-[40%] object-contain transform -translate-x-1/2 -translate-y-1/2"
                      src={treaty.image}
                      alt=""
                    />
                  )}
                </div>
                <div className="flex-1 text-black">
                  <p>{treaty.title}</p>
                  {treaty.info && <p className="font-normal">{treaty.info}</p>}
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
