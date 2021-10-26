import _ from 'lodash';

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
  name: string
): Poke<any> => viewAction({
  join: {
    resource: makeResource(ship, name),
    ship
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

export const hideGroup = (
  resource: string
): Poke<any> => viewAction({
  hide: resource
});

export const roleTags = ['janitor', 'moderator', 'admin'];
// TODO make this type better?

export const groupBunts = {
  group: (): Group => ({ members: new Set(), tags: { role: {} }, hidden: false, policy: groupBunts.policy() }),
  policy: (): GroupPolicy => ({ open: { banned: new Set(), banRanks: new Set() } })
};

export const joinError = ['no-perms', 'strange'] as const;
export const joinResult = ['done', ...joinError] as const;
export const joinProgress = ['start', 'added', ...joinResult] as const;

export const roleForShip = (
  group: Group,
  ship: PatpNoSig
): RoleTags | undefined => {
  return roleTags.reduce((currRole, role) => {
    const roleShips = group?.tags?.role?.[role];
    return roleShips && roleShips.has(ship) ? role : currRole;
  }, undefined as RoleTags | undefined);
}

export const resourceFromPath = (path: Path): Resource => {
  const [, , ship, name] = path.split('/');
  return { ship, name };
}

export const makeResource = (ship: string, name: string) => {
  return { ship, name };
}

export const isWriter = (group: Group, resource: string, ship: string) => {
  const writers: Set<string> | undefined = _.get(
    group,
    ['tags', 'graph', resource, 'writers'],
    undefined
  );
  const admins = group?.tags?.role?.admin ?? new Set();
  if (_.isUndefined(writers)) {
    return true;
  } else {
    return writers.has(ship) || admins.has(ship);
  }
}

export const isChannelAdmin = (
  group: Group,
  resource: string,
  ship: string
): boolean => {
  const role = roleForShip(group, ship.slice(1));

  return (
    isHost(resource, ship) ||
    role === 'admin' ||
    role === 'moderator'
  );
}

export const isHost = (
  resource: string,
  ship: string
): boolean => {
  const [, , host] = resource.split('/');

  return ship === host;
}
