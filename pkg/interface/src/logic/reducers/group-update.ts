import {
  Enc,
  Group,
  GroupUpdate,
  InvitePolicy, InvitePolicyDiff, OpenPolicy, OpenPolicyDiff, Tags
} from '@urbit/api';
import _ from 'lodash';
import { Cage } from '~/types/cage';
import { resourceAsPath } from '../lib/util';
import { BaseState } from '../state/base';
import { GroupState as State } from '../state/group';

type GroupState = BaseState<State> & State;

function decodeGroup(group: Enc<Group>): Group {
  return {
    ...group,
    tags: decodeTags(group.tags)
  };
}

function decodeTags(tags: Enc<Tags>): Tags {
  return _.reduce(
    tags,
    (acc, ships: any, key): Tags => {
      if (key.search(/\\/) === -1) {
        acc.role[key] = ships;
        return acc;
      } else {
        const [app, tag, resource] = key.split('\\');
        _.set(acc, [app, resource, tag], ships);
        return acc;
      }
    },
    { role: {} }
  );
}

export default class GroupReducer {
  reduce(json: Cage) {
    return;
  }
}
const initial = (json: GroupUpdate, state: GroupState): GroupState => {
  const data = json['initial'];
  if (data) {
    state.groups = _.mapValues(data, decodeGroup);
  }
  return state;
};

const initialGroup = (json: GroupUpdate, state: GroupState): GroupState => {
  if ('initialGroup' in json) {
    const { resource, group } = json.initialGroup;
    const path = resourceAsPath(resource);
    state.groups[path] = decodeGroup(group);
  }
  return state;
};

const addGroup = (json: GroupUpdate, state: GroupState): GroupState => {
  if ('addGroup' in json) {
    const { resource, policy, hidden } = json.addGroup;
    const resourcePath = resourceAsPath(resource);
    state.groups[resourcePath] = {
      members: [],
      tags: { role: { admin: [window.ship] } },
      policy,
      hidden
    };
  }
  return state;
};

const removeGroup = (json: GroupUpdate, state: GroupState): GroupState => {
  if('removeGroup' in json) {
    const { resource } = json.removeGroup;
    const resourcePath = resourceAsPath(resource);
    delete state.groups[resourcePath];
  }
  return state;
};

const addMembers = (json: GroupUpdate, state: GroupState): GroupState => {
  if ('addMembers' in json) {
    const { resource, ships } = json.addMembers;
    const resourcePath = resourceAsPath(resource);
    if(!(resourcePath in state.groups)) {
      return;
    }
    for (const member of ships) {
      const members = state.groups[resourcePath].members;
      if (!_.includes(members, member)) {
        members.push(member);
      }

      const policy = state.groups[resourcePath].policy;
      if ('invite' in policy) {
        const invites = (policy as InvitePolicy).invite;

        if (invites && _.includes(invites.pending, member)) {
          _.remove(invites.pending, item => item === member);
        }
      }
    }
  }
  return state;
};

const removeMembers = (json: GroupUpdate, state: GroupState): GroupState => {
  if ('removeMembers' in json) {
    const { resource, ships } = json.removeMembers;
    const resourcePath = resourceAsPath(resource);
    for (const member of ships) {
      _.remove(state.groups[resourcePath].members, item => item === member);
    }
  }
  return state;
};

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
};

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
};

const expose = (json: GroupUpdate, state: GroupState): GroupState => {
  if( 'expose' in json && state) {
    const { resource } = json.expose;
    const resourcePath = resourceAsPath(resource);
    state.groups[resourcePath].hidden = false;
  }
  return state;
};

const inviteChangePolicy = (diff: InvitePolicyDiff, policy: InvitePolicy) => {
  if ('addInvites' in diff) {
    const { addInvites } = diff;
    for (const ship of addInvites) {
      if (!_.includes(policy.invite.pending, ship)) {
        policy.invite.pending.push(ship);
      }
    }
  } else if ('removeInvites' in diff) {
    const { removeInvites } = diff;
    for (const ship of removeInvites) {
      _.remove(policy.invite.pending, item => item === ship);
    }
  } else {
    console.log('bad policy change');
  }
};

const openChangePolicy = (diff: OpenPolicyDiff, policy: OpenPolicy) => {
  if ('allowRanks' in diff) {
    const { allowRanks } = diff;
    for (const rank of allowRanks) {
      _.remove(policy.open.banRanks, item => item === rank);
    }
  } else if ('banRanks' in diff) {
    const { banRanks } = diff;
    for (const rank of banRanks) {
      _.remove(policy.open.banRanks, item => item === rank);
    }
  } else if ('allowShips' in diff) {
    const { allowShips } = diff;
    for (const ship of allowShips) {
      _.remove(policy.open.banned, item => item === ship);
    }
  } else if ('banShips' in diff) {
    const { banShips } = diff;
    for (const ship of banShips) {
      if (!_.includes(policy.open.banned, ship)) {
        policy.open.banned.push(ship);
      }
    }
  } else {
    console.log('bad policy change');
  }
};

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
};
export const reduce = [
  initial,
  addMembers,
  addTag,
  removeMembers,
  initialGroup,
  removeTag,
  addGroup,
  removeGroup,
  changePolicy,
  expose
];
