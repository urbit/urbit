import { Poke, Scry } from '../lib';
import { Pike } from './types';

export const getPikes: Scry = {
  app: 'hood',
  path: '/kiln/pikes'
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
 * Sync with a foreign desk
 */
export function kilnSync(
  ship: string,
  desk: string,
  local?: string
): Poke<any> {
  return {
    app: 'hood',
    mark: 'kiln-sync',
    json: {
      ship,
      desk,
      local: local || desk
    }
  };
}

/**
 * Unsync with a foreign desk
 */
export function kilnUnsync(
  ship: string,
  desk: string,
  local?: string
): Poke<any> {
  return {
    app: 'hood',
    mark: 'kiln-unsync',
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

export function kilnSuspend(
  desk: string
): Poke<any> {
  return {
    app: 'hood',
    mark: 'kiln-suspend',
    json: desk
  };
}

export function kilnRevive(
  desk: string
): Poke<any> {
  return {
    app: 'hood',
    mark: 'kiln-revive',
    json: desk
  };
}

export function kilnBump(): Poke<any> {
  return {
    app: 'hood',
    mark: 'kiln-bump',
    json: null,
  };
}

export function kilnPause(desk: string) {
  return {
    app: 'hood',
    mark: 'kiln-pause',
    json: desk
  };
}

export function kilnResume(desk: string) {
  return {
    app: 'hood',
    mark: 'kiln-resume',
    json: desk
  };
}

export const scryLag: Scry = ({ app: 'hood', path: '/kiln/lag' });

export function getPikePublisher(pike: Pike) {
  return pike.sync?.ship;
}
