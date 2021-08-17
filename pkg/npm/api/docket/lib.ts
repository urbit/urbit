import { Poke, Scry } from '../lib';

export const scryCharges: Scry = {
  app: 'docket',
  path: '/charges'
};

export const scryDockets: Scry = {
  app: 'docket',
  path: '/dockets'
};

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
