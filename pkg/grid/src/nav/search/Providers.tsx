import React, { useEffect, useMemo } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import fuzzy from 'fuzzy';
import { Provider, deSig } from '@urbit/api';
import * as ob from 'urbit-ob';
import { MatchItem, useLeapStore } from '../Nav';
import { useAllies, useCharges } from '../../state/docket';
import { ProviderList } from '../../components/ProviderList';
import useContactState from '../../state/contact';
import { AppList } from '../../components/AppList';
import { getAppHref } from '../../state/util';

type ProvidersProps = RouteComponentProps<{ ship: string }>;

export function providerMatch(provider: Provider | string): MatchItem {
  const value = typeof provider === 'string' ? provider : provider.shipName;
  const display = typeof provider === 'string' ? undefined : provider.nickname;

  return {
    value,
    display,
    url: `/leap/search/${value}/apps`,
    openInNewTab: false
  };
}

function fuzzySort(search: string) {
  return (a: fuzzy.FilterResult<string>, b: fuzzy.FilterResult<string>): number => {
    const left = a.string.startsWith(search) ? a.score + 1 : a.score;
    const right = b.string.startsWith(search) ? b.score + 1 : b.score;

    return right - left;
  };
}

export const Providers = ({ match }: ProvidersProps) => {
  const selectedMatch = useLeapStore((state) => state.selectedMatch);
  const provider = match?.params.ship;
  const contacts = useContactState((s) => s.contacts);
  const charges = useCharges();
  const allies = useAllies();
  const search = provider || '';
  const chargeArray = Object.entries(charges);
  const appResults = useMemo(
    () =>
      charges
        ? fuzzy
            .filter(
              search,
              chargeArray.map(([desk, charge]) => charge.title + desk)
            )
            .sort(fuzzySort(search))
            .map((el) => chargeArray[el.index][1])
        : [],
    [charges, search]
  );

  const patp = `~${deSig(search) || ''}`;
  const isValidPatp = ob.isValidPatp(patp);

  const results = useMemo(() => {
    if (!allies) {
      return [];
    }
    const exact =
      isValidPatp && !Object.keys(allies).includes(patp)
        ? [
            {
              shipName: patp,
              ...contacts[patp]
            }
          ]
        : [];
    return [
      ...exact,
      ...fuzzy
        .filter(
          search,
          Object.entries(allies).map(([ship]) => ship)
        )
        .sort(fuzzySort(search))
        .map((el) => ({ shipName: el.original, ...contacts[el.original] }))
    ];
  }, [allies, search, contacts]);

  const count = results?.length;

  useEffect(() => {
    if (search) {
      useLeapStore.setState({ rawInput: search });
    }
  }, []);

  useEffect(() => {
    if (results) {
      const providerMatches = results ? results.map(providerMatch) : [];
      const appMatches = appResults
        ? appResults.map((app) => ({
            url: getAppHref(app.href),
            openInNewTab: true,
            value: app.desk,
            display: app.title
          }))
        : [];

      const newProviderMatches = isValidPatp
        ? [
            {
              url: `/leap/search/${patp}/apps`,
              value: patp,
              display: patp,
              openInNewTab: false
            }
          ]
        : [];

      useLeapStore.setState({
        matches: ([] as MatchItem[]).concat(appMatches, providerMatches, newProviderMatches)
      });
    }
  }, [results, patp, isValidPatp]);

  return (
    <div
      className="dialog-inner-container md:px-6 md:py-8 space-y-0 h4 text-gray-400"
      aria-live="polite"
    >
      {appResults && !(results?.length > 0 && appResults.length === 0) && (
        <div>
          <h2 id="installed" className="mb-3">
            Installed Apps
          </h2>
          <AppList
            apps={appResults}
            labelledBy="installed"
            matchAgainst={selectedMatch}
            listClass="mb-6"
          />
        </div>
      )}
      {results && !(appResults?.length > 0 && results.length === 0) && (
        <div>
          <div id="providers">
            <h2 className="mb-1">Searching Software Providers</h2>
            <p className="mb-3">
              {count} result{count === 1 ? '' : 's'}
            </p>
          </div>
          <ProviderList
            providers={results}
            labelledBy="providers"
            matchAgainst={selectedMatch}
            listClass="mb-6"
          />
        </div>
      )}
      <p>That&apos;s it!</p>
    </div>
  );
};
