import { Enc, Path, Patp, PatpNoSig, Poke } from "..";
import {
  Group,
  GroupAction,
  GroupPolicyDiff,
  GroupUpdateAddMembers,
  GroupUpdateAddTag,
  GroupUpdateChangePolicy,
  GroupUpdateRemoveGroup,
  GroupUpdateRemoveMembers,
  GroupUpdateRemoveTag,
  Resource,
  Tag
} from "./index.d";

export const proxyAction = <T>(data: T): Poke<T> => ({
  app: 'group-push-hook',
  mark: 'group-update',
  json: data
});

export const storeAction = <T>(data: T): Poke<T> => ({
  app: 'group-store',
  mark: 'group-update',
  json: data
});

export const remove = (
  resource: Resource,
  ships: PatpNoSig[]
): Poke<GroupUpdateRemoveMembers> => proxyAction({
  removeMembers: {
    resource,
    ships
  }
});

export const addTag = (
  resource: Resource,
  tag: Tag,
  ships: Patp[]
): Poke<GroupUpdateAddTag> => proxyAction({
  addTag: {
    resource,
    tag,
    ships
  }
});

export const removeTag = (
  tag: Tag,
  resource: Resource,
  ships: PatpNoSig[]
): Poke<GroupUpdateRemoveTag> => proxyAction({
  removeTag: {
    tag,
    resource,
    ships
  }
});

export const add = (
  resource: Resource,
  ships: PatpNoSig[]
): Poke<GroupUpdateAddMembers> => proxyAction({
  addMembers: {
    resource,
    ships
  }
});

export const removeGroup = (
  resource: Resource
): Poke<GroupUpdateRemoveGroup> => storeAction({
  removeGroup: {
    resource
  }
});

export const changePolicy = (
  resource: Resource,
  diff: GroupPolicyDiff
): Poke<GroupUpdateChangePolicy> => proxyAction({
  changePolicy: {
    resource,
    diff
  }
});

export const roleTags = ['janitor', 'moderator', 'admin'];
// TODO make this type better?

export function roleForShip(group: Group, ship: PatpNoSig): string | undefined {
  return roleTags.reduce((currRole, role) => {
    const roleShips = group?.tags?.role?.[role];
    return roleShips && roleShips.has(ship) ? role : currRole;
  }, undefined as string | undefined);
}

export function resourceFromPath(path: Path): Resource {
  const [, , ship, name] = path.split('/');
  return { ship, name }
}

export function makeResource(ship: string, name:string) {
  return { ship, name };
}

export const joinError = ['no-perms', 'strange'] as const;
export const joinResult = ['done', ...joinError] as const;
export const joinProgress = ['start', 'added', ...joinResult] as const;