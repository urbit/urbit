import useMetadataState from '../state/metadata';
import ob from 'urbit-ob';
import useInviteState from '../state/invite';
import { deSig, Notification, resourceAsPath } from '@urbit/api';
import { createJoinParams } from '~/views/landscape/components/Join/Join';

function getGroupResourceRedirect(key: string) {
  const graphs = useMetadataState.getState().associations.graph;
  const association = graphs[`/ship/${key}`];
  if (!association || !('graph' in association.metadata.config)) {
    return '';
  }

  const section =
    association.group === association.resource
      ? '/messages'
      : association.group;
  return `/~landscape${section}/resource/${association.metadata.config.graph}${association.resource}`;
}

function getPostRedirect(key: string, segs: string[]) {
  const association =
    useMetadataState.getState().associations.graph[`/ship/${key}`];
  const { metadata } = association;
  if (!association || !('graph' in metadata.config)) {
    return '';
  }
  return `/~landscape${association.group}/feed/thread/${segs
    .slice(0, -1)
    .join('/')}`;
}

function getChatRedirect(chat: string, segs: string[]) {
  const qs = segs.length > 0 ? `?msg=${segs[0]}` : '';
  return `${getGroupResourceRedirect(chat)}${qs}`;
}

function getPublishRedirect(graphKey: string, segs: string[]) {
  const base = getGroupResourceRedirect(graphKey);
  if (segs.length === 3) {
    return `${base}/note/${segs[0]}`;
  } else if (segs.length === 4) {
    return `${base}/note/${segs[0]}?selected=${segs[2]}`;
  }
  return base;
}

function getLinkRedirect(graphKey: string, segs: string[]) {
  const base = getGroupResourceRedirect(graphKey);
  if (segs.length === 1) {
    return `${base}/index/${segs[0]}`;
  } else if (segs.length === 3) {
    return `${base}/index/${segs[0]}?selected=${segs[1]}`;
  }
  return base;
}

function getGraphRedirect(link: string) {
  const [, mark, ship, name, ...rest] = link.split('/');
  const graphKey = `${ship}/${name}`;
  switch (mark) {
    case 'graph-validator-dm':
      return `/~landscape/messages/dm/${ob.patp(rest[0])}`;
    case 'graph-validator-chat':
      return getChatRedirect(graphKey, rest);
    case 'graph-validator-publish':
      return getPublishRedirect(graphKey, rest);
    case 'graph-validator-link':
      return getLinkRedirect(graphKey, rest);
    case 'graph-validator-post':
      return getPostRedirect(graphKey, rest);
    default:
      return '';
  }
}

function getInviteRedirect(link: string) {
  const [, , app, uid] = link.split('/');
  const invite = useInviteState.getState().invites[app][uid];
  if (!invite || (app !== 'groups' && app !== 'graph')) {
    return '';
  }

  const { ship, name } = invite.resource;
  const alreadyJoined = getGroupResourceRedirect(`~${deSig(ship)}/${name}`);

  if (alreadyJoined) {
    return alreadyJoined;
  }

  return { search: createJoinParams(app, resourceAsPath(invite.resource)) };
}

function getDmRedirect(link: string) {
  const [, , ship] = link.split('/');
  return `/~landscape/messages/dm/${ship}`;
}
function getGroupRedirect(link: string) {
  const [, , ship, name] = link.split('/');
  return `/~landscape/ship/${ship}/${name}`;
}

export function getNotificationRedirectFromLink(link: string) {
  if (link.startsWith('/graph-validator')) {
    return getGraphRedirect(link);
  } else if (link.startsWith('/invite')) {
    return getInviteRedirect(link);
  } else if (link.startsWith('/dm')) {
    return getDmRedirect(link);
  } else if (link.startsWith('/groups')) {
    return getGroupRedirect(link);
  }
}

export function getNotificationRedirectFromPlacePath(
  notification: Notification
) {
  const placePath = notification.bin.place.path;
  switch (notification.bin.path) {
    case '/add-members':
      return `/~landscape/ship${placePath}`;
    case '/remove-members':
      return `/~landscape/ship${placePath}`;
    default:
      return undefined;
  }
}
