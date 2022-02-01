export const GROUP_UPDATE_VERSION = 0;
export const proxyAction = (data, version = GROUP_UPDATE_VERSION) => ({
    app: 'group-push-hook',
    mark: `group-update-${version}`,
    json: data
});
const storeAction = (data, version = GROUP_UPDATE_VERSION) => ({
    app: 'group-store',
    mark: `group-update-${version}`,
    json: data
});
export { storeAction as groupStoreAction };
const viewAction = (data) => ({
    app: 'group-view',
    mark: 'group-view-action',
    json: data
});
export { viewAction as groupViewAction };
export const viewThread = (thread, action) => ({
    inputMark: 'group-view-action',
    outputMark: 'json',
    threadName: thread,
    body: action
});
export const removeMembers = (resource, ships) => proxyAction({
    removeMembers: {
        resource,
        ships
    }
});
export const addTag = (resource, tag, ships) => proxyAction({
    addTag: {
        resource,
        tag,
        ships
    }
});
export const removeTag = (tag, resource, ships) => proxyAction({
    removeTag: {
        tag,
        resource,
        ships
    }
});
export const addMembers = (resource, ships) => proxyAction({
    addMembers: {
        resource,
        ships
    }
});
export const removeGroup = (resource) => storeAction({
    removeGroup: {
        resource
    }
});
export const changePolicy = (resource, diff) => proxyAction({
    changePolicy: {
        resource,
        diff
    }
});
export const makeResource = (ship, name) => {
    return { ship, name };
};
export const join = (ship, name, app, autojoin, share) => viewAction({
    join: {
        resource: makeResource(ship, name),
        ship,
        shareContact: share || false,
        app,
        autojoin
    }
});
export const createGroup = (name, policy, title, description) => viewThread('group-create', {
    create: {
        name,
        policy,
        title,
        description
    }
});
export const deleteGroup = (ship, name) => viewThread('group-delete', {
    remove: makeResource(ship, name)
});
export const leaveGroup = (ship, name) => viewThread('group-leave', {
    leave: makeResource(ship, name)
});
export const invite = (ship, name, ships, description) => viewThread('group-invite', {
    invite: {
        resource: makeResource(ship, name),
        ships,
        description
    }
});
export const abortJoin = (resource) => viewAction({
    abort: resource
});
export const roleTags = ['janitor', 'moderator', 'admin'];
// TODO make this type better?
export const groupBunts = {
    group: () => ({ members: new Set(), tags: { role: {} }, hidden: false, policy: groupBunts.policy() }),
    policy: () => ({ open: { banned: new Set(), banRanks: new Set() } })
};
export const joinError = ['no-perms', 'strange', 'abort'];
export const joinResult = ['done', ...joinError];
export const joinLoad = ['start', 'added', 'metadata'];
export const joinProgress = [...joinLoad, ...joinResult];
export const roleForShip = (group, ship) => {
    return roleTags.reduce((currRole, role) => {
        const roleShips = group?.tags?.role?.[role];
        return roleShips && roleShips.has(ship) ? role : currRole;
    }, undefined);
};
export const resourceFromPath = (path) => {
    const [, , ship, name] = path.split('/');
    return { ship, name };
};
export const isWriter = (group, resource, ship) => {
    const graph = group.tags?.graph;
    const writers = graph && graph[resource]?.writers;
    const admins = group?.tags?.role?.admin ?? new Set();
    if (typeof writers === 'undefined') {
        return true;
    }
    else {
        return writers.has(ship) || admins.has(ship);
    }
};
export const isHost = (resource, ship) => {
    const [, , host] = resource.split('/');
    return ship === host;
};
export const isChannelAdmin = (group, resource, ship) => {
    const role = roleForShip(group, ship.slice(1));
    return (isHost(resource, ship) ||
        role === 'admin' ||
        role === 'moderator');
};
//# sourceMappingURL=lib.js.map