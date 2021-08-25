import React, { MouseEvent, useCallback } from 'react';
import { Docket } from '@urbit/api';
import { MatchItem } from '../nav/Nav';
import { useRecentsStore } from '../nav/search/Home';
import { AppLink, AppLinkProps } from './AppLink';

type AppListProps<T extends Docket> = {
  apps: T[];
  labelledBy: string;
  matchAgainst?: MatchItem;
  onClick?: (e: MouseEvent<HTMLAnchorElement>, app: Docket) => void;
  listClass?: string;
} & Omit<AppLinkProps<T>, 'app' | 'onClick'>;

export function appMatches(target: Docket, match?: MatchItem): boolean {
  if (!match) {
    return false;
  }

  const matchValue = match.display || match.value;
  return target.title === matchValue; // TODO: need desk name or something || target.href === matchValue;
}

export const AppList = <T extends Docket>({
  apps,
  labelledBy,
  matchAgainst,
  onClick,
  listClass = 'space-y-8',
  ...props
}: AppListProps<T>) => {
  const addRecentApp = useRecentsStore((state) => state.addRecentApp);
  const selected = useCallback((app: Docket) => appMatches(app, matchAgainst), [matchAgainst]);

  return (
    <ul className={listClass} aria-labelledby={labelledBy}>
      {apps.map((app) => (
        <li key={app.title} id={app.title} role="option" aria-selected={selected(app)}>
          <AppLink
            {...props}
            app={app}
            selected={selected(app)}
            onClick={(e) => {
              addRecentApp(app);
              if (onClick) {
                onClick(e, app);
              }
            }}
          />
        </li>
      ))}
    </ul>
  );
};
