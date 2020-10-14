import { Path, PatpNoSig } from './noun';

export type PermissionUpdate =
  PermissionUpdateInitial
| PermissionUpdateCreate
| PermissionUpdateDelete
| PermissionUpdateRemove
| PermissionUpdateAdd;

interface PermissionUpdateInitial {
  initial: {
    [p in Path]: {
      who: PatpNoSig[];
      kind: PermissionKind;
    };
  }
}

interface PermissionUpdateCreate {
  create: {
    path: Path;
    kind: PermissionKind;
    who: PatpNoSig[];
  }
}

interface PermissionUpdateDelete {
  delete: {
    path: Path;
  }
}

interface PermissionUpdateAdd {
  add: {
    path: Path;
    who: PatpNoSig[];
  }
}

interface PermissionUpdateRemove {
  remove: {
    path: Path;
    who: PatpNoSig[];
  }
}

export type Permissions = {
  [p in Path]: Permission;
};
export interface Permission {
  who: Set<PatpNoSig>;
  kind: PermissionKind;
}

export type PermissionKind = 'white' | 'black';
