import React, { MouseEvent, useCallback } from 'react';
import { Contact, Provider } from '@urbit/api';
import classNames from 'classnames';
import { MatchItem } from '../nav/Nav';
import { useRecentsStore } from '../nav/search/Home';
import { ProviderLink, ProviderLinkProps } from './ProviderLink';

export type ProviderListProps = {
  providers: ({ shipName: string } & Contact)[];
  labelledBy: string;
  matchAgainst?: MatchItem;
  onClick?: (e: MouseEvent<HTMLAnchorElement>, p: Provider) => void;
  listClass?: string;
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
  listClass,
  size = 'default',
  ...props
}: ProviderListProps) => {
  const addRecentDev = useRecentsStore((state) => state.addRecentDev);
  const selected = useCallback(
    (provider: Provider) => providerMatches(provider, matchAgainst),
    [matchAgainst]
  );

  return (
    <ul
      className={classNames(size !== 'default' ? 'space-y-4' : 'space-y-8', listClass)}
      aria-labelledby={labelledBy}
    >
      {providers.map((p) => (
        <li key={p.shipName} id={p.shipName} role="option" aria-selected={selected(p)}>
          <ProviderLink
            {...props}
            size={size}
            provider={p}
            selected={selected(p)}
            onClick={(e) => {
              addRecentDev(p.shipName);
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
