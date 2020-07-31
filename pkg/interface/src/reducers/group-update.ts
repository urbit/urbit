import { StoreState } from '../store/type';
import { Cage } from '../types/cage';
import {
  GroupUpdate,
  Group,
  Tags,
  GroupPolicy,
  GroupPolicyDiff,
  OpenPolicyDiff,
  OpenPolicy,
  InvitePolicyDiff,
  InvitePolicy,
} from '../types/group-update';
import { Enc, PatpNoSig } from '../types/noun';
import { resourceAsPath } from '../lib/util';

type GroupState = Pick<StoreState, 'groups' | 'groupKeys'>;

function decodeGroup(group: Enc<Group>): Group {
  const members = new Set(group.members);
  const res = {
    ...group,
    members,
    tags: decodeTags(group.tags),
    policy: decodePolicy(group.policy),
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
      open: { banned: new Set(open.banned), banRanks: new Set(open.banRanks) },
    };
  }
}

function decodeTags(tags: Enc<Tags>): Tags {
  return Object.entries(tags)
    .reduce((acc, [key, tag]) => {
      if (Array.isArray(tag)) {
        acc.role[key] = new Set(tag);
        return acc;
      } else {
        const app = Object.entries(tag)
          .reduce((inner, [k, t]) => {
            inner[k] = new Set(t);
            return inner;
          }, {});
        acc[key] = app;
        return acc;
      }
    }, { role: {} });
}

export default class GroupReducer<S extends GroupState> {
  reduce(json: Cage, state: S) {
    const data = json.groupUpdate;
    if (data) {
      this.initial(data, state);
      this.addMembers(data, state);
      this.addTag(data, state);
      this.removeMembers(data, state);
      this.initialGroup(data, state);
      this.removeTag(data, state);
      this.initial(data, state);
      this.addGroup(data, state);
      this.removeGroup(data, state);
      this.changePolicy(data, state);
    }
  }

  initial(json: GroupUpdate, state: S) {
    const data = json['initial'];
    if (data) {
      state.groups = Object.entries(data).reduce((acc, [key, value]) => {
        acc[key] = decodeGroup(value);
        return acc;
      }, {});
    }
  }

  initialGroup(json: GroupUpdate, state: S) {
    if ('initialGroup' in json) {
      const { resource, group } = json.initialGroup;
      const path = resourceAsPath(resource);
      state.groups[path] = decodeGroup(group);
    }
  }

  addGroup(json: GroupUpdate, state: S) {
    if ('addGroup' in json) {
      const { resource, policy, hidden } = json.addGroup;
      const resourcePath = resourceAsPath(resource);
      state.groups[resourcePath] = {
        members: new Set(),
        tags: { role: {} },
        policy: decodePolicy(policy),
        hidden,
      };
    }
  }
  removeGroup(json: GroupUpdate, state: S) {
    if('removeGroup' in json) {
      const { resource } = json.removeGroup;
      const resourcePath = resourceAsPath(resource);
      delete state.groups[resourcePath];
    }
  }

  addMembers(json: GroupUpdate, state: S) {
    if ('addMembers' in json) {
      const { resource, ships } = json.addMembers;
      const resourcePath = resourceAsPath(resource);
      for (const member of ships) {
        state.groups[resourcePath].members.add(member);
      }
    }
  }

  removeMembers(json: GroupUpdate, state: S) {
    if ('removeMembers' in json) {
      const { resource, ships } = json.removeMembers;
      const resourcePath = resourceAsPath(resource);
      for (const member of ships) {
        state.groups[resourcePath].members.delete(member);
      }
    }
  }

  addTag(json: GroupUpdate, state: S) {
    if ('addTag' in json) {
      const { resource, tag, ships } = json.addTag;
      const resourcePath = resourceAsPath(resource);
      const tags = state.groups[resourcePath].tags;
      const tagged = tags[tag['app'] || 'role'][tag.tag] || new Set();
      for (const ship of ships) {
        tagged.add(ship);
      }
      tags[tag['app'] || 'role'][tag.tag] = tagged;
    }
  }

  removeTag(json: GroupUpdate, state: S) {
    if ('removeTag' in json) {
      const { resource, tag, ships } = json.removeTag;
      const resourcePath = resourceAsPath(resource);
      const tags = state.groups[resourcePath].tags;
      const tagged = tags[tag['app'] || 'role'][tag.tag] || new Set();

      if (!tagged) {
        return;
      }
      for (const ship of ships) {
        tagged.delete(ship);
      }
      tags[tag['app'] || 'role'][tag.tag] = tagged;
    }
  }

  changePolicy(json: GroupUpdate, state: S) {
    if ('changePolicy' in json && state) {
      const { resource, diff } = json.changePolicy;
      const resourcePath = resourceAsPath(resource);
      const policy = state.groups[resourcePath].policy;
      if ('open' in policy && 'open' in diff) {
        this.openChangePolicy(diff.open, policy);
      } else if ('invite' in policy && 'invite' in diff) {
        this.inviteChangePolicy(diff.invite, policy);
      } else if ('replace' in diff) {
        state.groups[resourcePath].policy = diff.replace;
      } else {
        console.log('bad policy diff');
      }
    }
  }

  private inviteChangePolicy(diff: InvitePolicyDiff, policy: InvitePolicy) {
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

  private openChangePolicy(diff: OpenPolicyDiff, policy: OpenPolicy) {
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
      console.log('allowing ships');
      const { allowShips } = diff;
      for (const ship of allowShips) {
        policy.open.banned.delete(ship);
      }
    } else if ('banShips' in diff) {
      console.log('banning ships');
      const { banShips } = diff;
      for (const ship of banShips) {
        policy.open.banned.add(ship);
      }
    } else {
      console.log('bad policy change');
    }
  }
}
