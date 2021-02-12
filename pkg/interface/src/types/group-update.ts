import { PatpNoSig, Path, Jug, ShipRank, Enc } from './noun';

export const roleTags = ['janitor', 'moderator', 'admin'] as const;
export type RoleTags = typeof roleTags[number];
interface RoleTag {
  tag: 'admin' | 'moderator' | 'janitor';
}

interface AppTag {
  app: string;
  resource: string;
  tag: string;
}

export type Tag = AppTag | RoleTag;

export interface InvitePolicy {
  invite: {
    pending: Set<PatpNoSig>;
  };
}

export interface OpenPolicy {
  open: {
    banned: Set<PatpNoSig>;
    banRanks: Set<ShipRank>;
  };
}

export interface Resource {
  name: string;
  ship: PatpNoSig;
}

export type OpenPolicyDiff =
  | AllowRanksDiff
  | BanRanksDiff
  | AllowShipsDiff
  | BanShipsDiff;

interface AllowRanksDiff {
  allowRanks: ShipRank[];
}

interface BanRanksDiff {
  banRanks: ShipRank[];
}

interface AllowShipsDiff {
  allowShips: PatpNoSig[];
}

interface BanShipsDiff {
  banShips: PatpNoSig[];
}

export type InvitePolicyDiff = AddInvitesDiff | RemoveInvitesDiff;

interface AddInvitesDiff {
  addInvites: PatpNoSig[];
}

interface RemoveInvitesDiff {
  removeInvites: PatpNoSig[];
}

interface ReplacePolicyDiff {
  replace: GroupPolicy;
}

export type GroupPolicyDiff =
  | { open: OpenPolicyDiff }
  | { invite: InvitePolicyDiff }
  | ReplacePolicyDiff;

export type GroupPolicy = OpenPolicy | InvitePolicy;

interface TaggedShips {
  [tag: string]: Set<PatpNoSig>;
}

export interface Tags {
  role: TaggedShips;
  [app: string]: TaggedShips;
}

export interface Group {
  members: Set<PatpNoSig>;
  tags: Tags;
  policy: GroupPolicy;
  hidden: boolean;
}

export type Groups = {
  [p in Path]: Group;
};

interface GroupUpdateInitial {
  initial: Enc<Groups>;
}

interface GroupUpdateAddGroup {
  addGroup: {
    resource: Resource;
    policy: Enc<GroupPolicy>;
    hidden: boolean;
  };
}

interface GroupUpdateAddMembers {
  addMembers: {
    ships: PatpNoSig[];
    resource: Resource;
  };
}

interface GroupUpdateRemoveMembers {
  removeMembers: {
    ships: PatpNoSig[];
    resource: Resource;
  };
}

interface GroupUpdateAddTag {
  addTag: {
    tag: Tag;
    resource: Resource;
    ships: PatpNoSig[];
  };
}

interface GroupUpdateRemoveTag {
  removeTag: {
    tag: Tag;
    resource: Resource;
    ships: PatpNoSig;
  };
}

interface GroupUpdateChangePolicy {
  changePolicy: { resource: Resource; diff: GroupPolicyDiff };
}

interface GroupUpdateRemoveGroup {
  removeGroup: {
    resource: Resource;
  };
}

interface GroupUpdateExpose {
  expose: {
    resource: Resource;
  };
}

interface GroupUpdateInitialGroup {
  initialGroup: {
    resource: Resource;
    group: Enc<Group>;
  };
}

export type GroupUpdate =
  | GroupUpdateInitial
  | GroupUpdateAddGroup
  | GroupUpdateAddMembers
  | GroupUpdateRemoveMembers
  | GroupUpdateAddTag
  | GroupUpdateRemoveTag
  | GroupUpdateChangePolicy
  | GroupUpdateRemoveGroup
  | GroupUpdateExpose
  | GroupUpdateInitialGroup;

export type GroupAction = Omit<GroupUpdate, 'initialGroup' | 'initial'>;

export const groupBunts = {
  group: (): Group => ({ members: new Set(), tags: { role: {} }, hidden: false, policy: groupBunts.policy() }),
  policy: (): GroupPolicy => ({ open: { banned: new Set(), banRanks: new Set() } })
};
