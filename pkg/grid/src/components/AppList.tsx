import React, { MouseEvent, useCallback } from 'react';
import { Docket } from '@urbit/api';
import { MatchItem } from '../nav/Nav';
import { useRecentsStore } from '../nav/search/Home';
import { AppLink, AppLinkProps } from './AppLink';

type AppListProps = {
  apps: Docket[];
  labelledBy: string;
  matchAgainst?: MatchItem;
  onClick?: (e: MouseEvent<HTMLAnchorElement>, app: Docket) => void;
} & Omit<AppLinkProps, 'app' | 'onClick'>;

export function appMatches(target: Docket, match?: MatchItem): boolean {
  if (!match) {
    return false;
  }

  const matchValue = match.display || match.value;
  return target.title === matchValue; // TODO: need desk name or something || target.href === matchValue;
}

export const AppList = ({ apps, labelledBy, matchAgainst, onClick, ...props }: AppListProps) => {
  const addRecentApp = useRecentsStore((state) => state.addRecentApp);
  const selected = useCallback((app: Docket) => appMatches(app, matchAgainst), [matchAgainst]);

  return (
    <ul className="space-y-8" aria-labelledby={labelledBy}>
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
