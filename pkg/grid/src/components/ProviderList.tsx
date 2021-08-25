import React, { MouseEvent, useCallback } from 'react';
import { Provider } from '@urbit/api';
import { MatchItem } from '../nav/Nav';
import { useRecentsStore } from '../nav/search/Home';
import { ProviderLink, ProviderLinkProps } from './ProviderLink';

export type ProviderListProps = {
  providers: Provider[];
  labelledBy: string;
  matchAgainst?: MatchItem;
  onClick?: (e: MouseEvent<HTMLAnchorElement>, p: Provider) => void;
} & Omit<ProviderLinkProps, 'provider' | 'onClick'>;

export function providerMatches(target: Provider, match?: MatchItem): boolean {
  if (!match) {
    return false;
  }

  const matchValue = match.display || match.value;
  return target.nickname === matchValue || target.shipName === matchValue;
}

export const ProviderList = ({
  providers,
  labelledBy,
  matchAgainst,
  onClick,
  ...props
}: ProviderListProps) => {
  const addRecentDev = useRecentsStore((state) => state.addRecentDev);
  const selected = useCallback(
    (provider: Provider) => providerMatches(provider, matchAgainst),
    [matchAgainst]
  );

  return (
    <ul className="space-y-8" aria-labelledby={labelledBy}>
      {providers.map((p) => (
        <li key={p.shipName} id={p.shipName} role="option" aria-selected={selected(p)}>
          <ProviderLink
            {...props}
            provider={p}
            selected={selected(p)}
            onClick={(e) => {
              addRecentDev(p);
              if (onClick) {
                onClick(e, p);
              }
            }}
          />
        </li>
      ))}
    </ul>
  );
};
