import React, { MouseEvent, useCallback } from 'react';
import classNames from 'classnames';
import { MatchItem } from '../nav/Nav';
import { useRecentsStore } from '../nav/search/Home';
import { AppLink, AppLinkProps } from './AppLink';
import { DocketWithDesk } from '../state/docket';

type AppListProps<T extends DocketWithDesk> = {
  apps: T[];
  labelledBy: string;
  matchAgainst?: MatchItem;
  onClick?: (e: MouseEvent<HTMLAnchorElement>, app: T) => void;
  listClass?: string;
} & Omit<AppLinkProps, 'app' | 'onClick'>;

export function appMatches(target: DocketWithDesk, match?: MatchItem): boolean {
  if (!match) {
    return false;
  }

  const matchValue = match.display || match.value;
  return target.title === matchValue || target.desk === matchValue;
}

export const AppList = <T extends DocketWithDesk>({
  apps,
  labelledBy,
  matchAgainst,
  onClick,
  listClass,
  size = 'default',
  ...props
}: AppListProps<T>) => {
  const addRecentApp = useRecentsStore((state) => state.addRecentApp);
  const selected = useCallback((app: T) => appMatches(app, matchAgainst), [matchAgainst]);

  return (
    <ul
      className={classNames(
        size === 'default' && 'space-y-4',
        size !== 'xs' && '-mx-2',
        size === 'xs' && '-mx-1',
        listClass
      )}
      aria-labelledby={labelledBy}
    >
      {apps.map((app) => (
        <li key={app.title} id={app.title} role="option" aria-selected={selected(app)}>
          <AppLink
            {...props}
            app={app}
            size={size}
            selected={selected(app)}
            onClick={(e) => {
              addRecentApp(app);
              onClick?.(e, app);
            }}
          />
        </li>
      ))}
    </ul>
  );
};
