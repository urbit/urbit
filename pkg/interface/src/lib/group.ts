import { roleTags, RoleTags, Group, Resource } from '../types/group-update';
import { PatpNoSig, Path } from '../types/noun';


export function roleForShip(group: Group, ship: PatpNoSig): RoleTags | undefined {
  return roleTags.reduce((currRole, role) => {
    const roleShips = group.tags.role[role];
    return roleShips && roleShips.has(ship) ? role : currRole;
  }, undefined as RoleTags | undefined);
}

export function resourceFromPath(path: Path): Resource {
  const [, , ship, name] = path.split('/');
  return { ship, name }
}
