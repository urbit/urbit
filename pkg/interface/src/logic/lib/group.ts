import { Path, PatpNoSig } from '@urbit/api';
import { Group, Resource, roleTags, RoleTags } from '@urbit/api/groups';
import _ from 'lodash';

export function roleForShip(
  group: Group,
  ship: PatpNoSig
): RoleTags | undefined {
  return roleTags.reduce((currRole, role) => {
    const roleShips = group?.tags?.role?.[role];
    return roleShips && roleShips.has(ship) ? role : currRole;
  }, undefined as RoleTags | undefined);
}

export function resourceFromPath(path: Path): Resource {
  const [, , ship, name] = path.split('/');
  return { ship, name };
}

export function makeResource(ship: string, name: string) {
  return { ship, name };
}

export function isWriter(group: Group, resource: string) {
  const writers: Set<string> | undefined = _.get(
    group,
    ['tags', 'graph', resource, 'writers'],
    undefined
  );
  const admins = group?.tags?.role?.admin ?? new Set();
  if (_.isUndefined(writers)) {
    return true;
  } else {
    return writers.has(window.ship) || admins.has(window.ship);
  }
}

export function isChannelAdmin(group: Group, resource: string, ship = `~${window.ship}`) {
  const role = roleForShip(group, ship.slice(1));

  return (
    isHost(resource, ship) ||
    role === 'admin' ||
    role === 'moderator'
  );
}

export function isHost(resource: string, ship = `~${window.ship}`) {
  const [, , host] = resource.split('/');

  return ship === host;
}
