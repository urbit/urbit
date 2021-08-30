import { Poke, Scry } from '../lib';
import { Chad } from './types';

export function chadIsRunning(chad: Chad) {
  return 'glob' in chad || 'site' in chad;
}

export const scryCharges: Scry = {
  app: 'docket',
  path: '/charges'
};

export const scryDockets: Scry = {
  app: 'docket',
  path: '/dockets'
};

export const scryTreaties: Scry = {
  app: 'treaty',
  path: '/treaties'
};

export const scryAllies: Scry = {
  app: 'treaty',
  path: '/allies'
};

export const scryAllyTreaties = (ship: string): Scry => ({
  app: 'treaty',
  path: `/treaties/${ship}`
});

/**
 * Uninstall a desk, and remove docket
 */
export function docketUninstall(desk: string): Poke<string> {
  return {
    app: 'docket',
    mark: 'docket-uninstall',
    json: desk
  };
}

export function docketInstall(ship: string, desk: string): Poke<any> {
  return {
    app: 'docket',
    mark: 'docket-install',
    json: `${ship}/${desk}`
  };
}
