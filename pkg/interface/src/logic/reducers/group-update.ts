import _ from 'lodash';
import { Cage } from '~/types/cage';
import {
  GroupUpdate,
  Group,
  Tags,
  GroupPolicy,
  GroupPolicyDiff,
  OpenPolicyDiff,
  OpenPolicy,
  InvitePolicyDiff,
  InvitePolicy
} from '@urbit/api/groups';
import { Enc, PatpNoSig } from '@urbit/api';
import { resourceAsPath } from '../lib/util';
import useGroupState, { GroupState } from '../state/group';
import { compose } from 'lodash/fp';
import { reduceState } from '../state/base';

function decodeGroup(group: Enc<Group>): Group {
  const members = new Set(group.members);
  const res = {
    ...group,
    members,
    tags: decodeTags(group.tags),
    policy: decodePolicy(group.policy)
  };
  return res;
}

function decodePolicy(policy: Enc<GroupPolicy>): GroupPolicy {
  if ('invite' in policy) {
    const { invite } = policy;
    return { invite: { pending: new Set(invite.pending) } };
  } else {
    const { open } = policy;
    return {
      open: { banned: new Set(open.banned), banRanks: new Set(open.banRanks) }
    };
  }
}

function decodeTags(tags: Enc<Tags>): Tags {
  return _.reduce(
    tags,
    (acc, ships, key): Tags => {
      if (key.search(/\\/) === -1) {
        acc.role[key] = new Set(ships);
        return acc;
      } else {
        const [app, tag, resource] = key.split('\\');
        _.set(acc, [app, resource, tag], new Set(ships));
        return acc;
      }
    },
    { role: {} }
  );
}

export default class GroupReducer {
  reduce(json: Cage) {
    const data = json.groupUpdate;
    if (data) {
      reduceState<GroupState, GroupUpdate>(useGroupState, data, [
        initial,
        addMembers,
        addTag,
        removeMembers,
        initialGroup,
        removeTag,
        addGroup,
        removeGroup,
        changePolicy,
        expose,
      ]);
    }
  }

}
const initial = (json: GroupUpdate, state: GroupState): GroupState => {
  const data = json['initial'];
  if (data) {
    state.groups = _.mapValues(data, decodeGroup);
  }
  return state;
}

const initialGroup = (json: GroupUpdate, state: GroupState): GroupState => {
  if ('initialGroup' in json) {
    const { resource, group } = json.initialGroup;
    const path = resourceAsPath(resource);
    state.groups[path] = decodeGroup(group);
  }
  return state;
}

const addGroup = (json: GroupUpdate, state: GroupState): GroupState => {
  if ('addGroup' in json) {
    const { resource, policy, hidden } = json.addGroup;
    const resourcePath = resourceAsPath(resource);
    state.groups[resourcePath] = {
      members: new Set(),
      tags: { role: { admin: new Set([window.ship]) } },
      policy: decodePolicy(policy),
      hidden
    };
  }
  return state;
}

const removeGroup = (json: GroupUpdate, state: GroupState): GroupState => {
  if('removeGroup' in json) {
    const { resource } = json.removeGroup;
    const resourcePath = resourceAsPath(resource);
    delete state.groups[resourcePath];
  }
  return state;
}

const addMembers = (json: GroupUpdate, state: GroupState): GroupState => {
  if ('addMembers' in json) {
    const { resource, ships } = json.addMembers;
    const resourcePath = resourceAsPath(resource);
    for (const member of ships) {
      state.groups[resourcePath].members.add(member);
      if (
          'invite' in state.groups[resourcePath].policy &&
          state.groups[resourcePath].policy.invite.pending.has(member)
         ) {
           state.groups[resourcePath].policy.invite.pending.delete(member)
         }
    }
  }
  return state;
}

const removeMembers = (json: GroupUpdate, state: GroupState): GroupState => {
  if ('removeMembers' in json) {
    const { resource, ships } = json.removeMembers;
    const resourcePath = resourceAsPath(resource);
    for (const member of ships) {
      state.groups[resourcePath].members.delete(member);
    }
  }
  return state;
}

const addTag = (json: GroupUpdate, state: GroupState): GroupState => {
  if ('addTag' in json) {
    const { resource, tag, ships } = json.addTag;
    const resourcePath = resourceAsPath(resource);
    const tags = state.groups[resourcePath].tags;
    const tagAccessors =
      'app' in tag ? [tag.app,tag.resource, tag.tag] :  ['role', tag.tag];
    const tagged = _.get(tags, tagAccessors, new Set());
    for (const ship of ships) {
      tagged.add(ship);
    }
    _.set(tags, tagAccessors, tagged);
  }
  return state;
}

const removeTag = (json: GroupUpdate, state: GroupState): GroupState => {
  if ('removeTag' in json) {
    const { resource, tag, ships } = json.removeTag;
    const resourcePath = resourceAsPath(resource);
    const tags = state.groups[resourcePath].tags;
    const tagAccessors =
      'app' in tag ? [tag.app, tag.resource, tag.tag] :  ['role', tag.tag];
    const tagged = _.get(tags, tagAccessors, new Set());

    if (!tagged) {
      return state;
    }
    for (const ship of ships) {
      tagged.delete(ship);
    }
    _.set(tags, tagAccessors, tagged);
  }
  return state;
}

const changePolicy = (json: GroupUpdate, state: GroupState): GroupState => {
  if ('changePolicy' in json && state) {
    const { resource, diff } = json.changePolicy;
    const resourcePath = resourceAsPath(resource);
    const policy = state.groups[resourcePath].policy;
    if ('open' in policy && 'open' in diff) {
      openChangePolicy(diff.open, policy);
    } else if ('invite' in policy && 'invite' in diff) {
      inviteChangePolicy(diff.invite, policy);
    } else if ('replace' in diff) {
      state.groups[resourcePath].policy = diff.replace;
    } else {
      console.log('bad policy diff');
    }
  }
  return state;
}

const expose = (json: GroupUpdate, state: GroupState): GroupState => {
  if( 'expose' in json && state) {
    const { resource } = json.expose;
    const resourcePath = resourceAsPath(resource);
    state.groups[resourcePath].hidden = false;
  }
  return state;
}

const inviteChangePolicy = (diff: InvitePolicyDiff, policy: InvitePolicy) => {
  if ('addInvites' in diff) {
    const { addInvites } = diff;
    for (const ship of addInvites) {
      policy.invite.pending.add(ship);
    }
  } else if ('removeInvites' in diff) {
    const { removeInvites } = diff;
    for (const ship of removeInvites) {
      policy.invite.pending.delete(ship);
    }
  } else {
    console.log('bad policy change');
  }
}

const openChangePolicy = (diff: OpenPolicyDiff, policy: OpenPolicy) => {
  if ('allowRanks' in diff) {
    const { allowRanks } = diff;
    for (const rank of allowRanks) {
      policy.open.banRanks.delete(rank);
    }
  } else if ('banRanks' in diff) {
    const { banRanks } = diff;
    for (const rank of banRanks) {
      policy.open.banRanks.delete(rank);
    }
  } else if ('allowShips' in diff) {
    const { allowShips } = diff;
    for (const ship of allowShips) {
      policy.open.banned.delete(ship);
    }
  } else if ('banShips' in diff) {
    const { banShips } = diff;
    for (const ship of banShips) {
      policy.open.banned.add(ship);
    }
  } else {
    console.log('bad policy change');
  }
}
