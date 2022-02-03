import { decToUd } from '../lib';
export const harkAction = (data) => ({
    app: 'hark-store',
    mark: 'hark-action',
    json: data
});
const graphHookAction = (data) => ({
    app: 'hark-graph-hook',
    mark: 'hark-graph-hook-action',
    json: data
});
export { graphHookAction as harkGraphHookAction };
const groupHookAction = (data) => ({
    app: 'hark-group-hook',
    mark: 'hark-group-hook-action',
    json: data
});
export { groupHookAction as harkGroupHookAction };
export const actOnNotification = (frond, intTime, bin) => harkAction({
    [frond]: {
        time: decToUd(intTime.toString()),
        bin
    }
});
export const setMentions = (mentions) => graphHookAction({
    'set-mentions': mentions
});
export const setWatchOnSelf = (watchSelf) => graphHookAction({
    'set-watch-on-self': watchSelf
});
export const setDoNotDisturb = (dnd) => harkAction({
    'set-dnd': dnd
});
export const archive = (bin, lid) => harkAction({
    archive: {
        lid,
        bin
    }
});
export const opened = harkAction({
    opened: null
});
export const markCountAsRead = (place) => harkAction({
    'read-count': place
});
export const markEachAsRead = (place, path) => harkAction({
    'read-each': {
        place,
        path
    }
});
export const seen = () => harkAction({ seen: null });
export const readAll = harkAction({ 'read-all': null });
export const archiveAll = harkAction({ 'archive-all': null });
export const ignoreGroup = (group) => groupHookAction({
    ignore: group
});
export const ignoreGraph = (graph, index) => graphHookAction({
    ignore: {
        graph,
        index
    }
});
export const listenGroup = (group) => groupHookAction({
    listen: group
});
export const listenGraph = (graph, index) => graphHookAction({
    listen: {
        graph,
        index
    }
});
/**
 * Read all graphs belonging to a particular group
 */
export const readGroup = (group) => harkAction({
    'read-group': group
});
/**
 * Read all unreads in a graph
 */
export const readGraph = (graph) => harkAction({
    'read-graph': graph
});
export function harkBinToId(bin) {
    const { place, path } = bin;
    return `${place.desk}${place.path}${path}`;
}
export function harkBinEq(a, b) {
    return (a.place.path === b.place.path &&
        a.place.desk === b.place.desk &&
        a.path === b.path);
}
export function harkLidToId(lid) {
    if ('time' in lid) {
        return `archive-${lid.time}`;
    }
    return Object.keys(lid)[0];
}
//# sourceMappingURL=lib.js.map