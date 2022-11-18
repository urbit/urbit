import { deSig } from '../index';
import { Enc, Path, Patp, PatpNoSig, Poke, Thread } from '../lib/types';
import { Group, GroupPolicy, GroupPolicyDiff, GroupUpdateAddMembers, GroupUpdateAddTag, GroupUpdateChangePolicy, GroupUpdateRemoveGroup, GroupUpdateRemoveMembers, GroupUpdateRemoveTag, Resource, RoleTags, Tag } from './types';
import { GroupUpdate } from './update';

export const GROUP_UPDATE_VERSION = 0;

export const proxyAction = <T>(data: T, version: number = GROUP_UPDATE_VERSION): Poke<T> => ({
  app: 'group-push-hook',
  mark: `group-update-${version}`,
  json: data
});

const storeAction = <T extends GroupUpdate>(data: T, version: number = GROUP_UPDATE_VERSION): Poke<T> => ({
  app: 'group-store',
  mark: `group-update-${version}`,
  json: data
});

export { storeAction as groupStoreAction };

const viewAction = <T>(data: T): Poke<T> => ({
  app: 'group-view',
  mark: 'group-view-action',
  json: data
});

export { viewAction as groupViewAction };

export const viewThread = <T>(thread: string, action: T): Thread<T> => ({
  inputMark: 'group-view-action',
  outputMark: 'json',
  threadName: thread,
  body: action
});

export const removeMembers = (
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

export const addMembers = (
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
  diff: Enc<GroupPolicyDiff>
): Poke<Enc<GroupUpdateChangePolicy>> => proxyAction({
  changePolicy: {
    resource,
    diff
  }
});

export const join = (
  ship: string,
  name: string,
  app: "groups" | "graph",
  autojoin: boolean,
  share: boolean
): Poke<any> => viewAction({
  join: {
    resource: makeResource(ship, name),
    ship,
    shareContact: share || false,
    app,
    autojoin
  }
});

export const createGroup = (
  name: string,
  policy: Enc<GroupPolicy>,
  title: string,
  description: string
): Thread<any> => viewThread('group-create', {
  create: {
    name,
    policy,
    title,
    description
  }
});

export const deleteGroup = (
  ship: string,
  name: string
): Thread<any> => viewThread('group-delete', {
  remove: makeResource(ship, name)
});

export const leaveGroup = (
  ship: string,
  name: string
): Thread<any> => viewThread('group-leave', {
  leave: makeResource(ship, name)
});

export const invite = (
  ship: string,
  name: string,
  ships: Patp[],
  description: string
): Thread<any> => viewThread('group-invite', {
  invite: {
    resource: makeResource(ship, name),
    ships,
    description
  }
});

export const abortJoin = (
  resource: string
): Poke<any> => viewAction({
  abort: resource
});

export const roleTags = ['janitor', 'moderator', 'admin'];
// TODO make this type better?

export const groupBunts = {
  group: (): Group => ({ members: [], tags: { role: {} }, hidden: false, policy: groupBunts.policy() }),
  policy: (): GroupPolicy => ({ open: { banned: [], banRanks: [] } })
};

export const joinError = ['no-perms', 'strange', 'abort'] as const;
export const joinResult = ['done', ...joinError] as const;
export const joinLoad = ['start', 'added', 'metadata'] as const;
export const joinProgress = [...joinLoad, ...joinResult] as const;

export function roleForShip(
  group: Group,
  ship: PatpNoSig
): RoleTags | undefined {
  return roleTags.reduce((currRole, role) => {
    const roleShips = group?.tags?.role?.[role];
    return roleShips && roleShips.includes(ship) ? role : currRole;
  }, undefined as RoleTags | undefined);
};

export function resourceFromPath(path: Path): Resource {
  const [, , ship, name] = path.split('/');
  return { ship, name };
}

export function makeResource(ship: string, name: string) {
  return { ship, name };
}

export const isWriter = (group: Group, resource: string, ship: string) => {
  const graph = group?.tags?.graph;
  const writers: string[] | undefined = graph && (graph[resource] as any)?.writers;
  const admins = group?.tags?.role?.admin ?? [];
  if (typeof writers === 'undefined') {
    return true;
  } else {
    return [...writers].includes(ship) || admins.includes(ship);
  }
};

export function isChannelAdmin(
  group: Group,
  resource: string,
  ship: string
): boolean {
  const role = roleForShip(group, deSig(ship));

  return (
    isHost(resource, ship) ||
    role === 'admin' ||
    role === 'moderator'
  );
}

export function isHost(
  resource: string,
  ship: string
): boolean {
  const [, , host] = resource.split('/');

  return ship === host;
}
