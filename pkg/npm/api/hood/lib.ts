import { Poke, Scry } from '../lib';

export const getVats: Scry = {
  app: 'hood',
  path: '/kiln/vats'
};

/**
 * Install a foreign desk
 */
export function kilnInstall(
  ship: string,
  desk: string,
  local?: string
): Poke<any> {
  return {
    app: 'hood',
    mark: 'kiln-install',
    json: {
      ship,
      desk,
      local: local || desk
    }
  };
}

/**
 * Uninstall a desk
 */
export function kilnUninstall(
  desk: string
): Poke<any> {
  return {
    app: 'hood',
    mark: 'kiln-uninstall',
    json: desk
  };
}
