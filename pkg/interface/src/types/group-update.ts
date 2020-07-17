import { PatpNoSig, Path } from './noun';

export type Group = Set<PatpNoSig>

export type Groups = {
  [p in Path]: Group;
}

interface GroupUpdateInitial {
  initial: Groups;
}

interface GroupUpdateAdd {
  add: {
    members: PatpNoSig[];
    path: Path;
  }
}

interface GroupUpdateRemove {
  remove: {
    members: PatpNoSig[];
    path: Path;
  }
}

interface GroupUpdateBundle {
  bundle: {
    path: Path;
  }
}

interface GroupUpdateUnbundle {
  unbundle: {
    path: Path;
  }
}

interface GroupUpdateKeys {
  keys: {
    keys: Path[];
  }
}

interface GroupUpdatePath {
  path: {
    path: Path;
    members: PatpNoSig[];
  }
}

export type GroupUpdate =
  GroupUpdateInitial
| GroupUpdateAdd
| GroupUpdateRemove
| GroupUpdateBundle
| GroupUpdateUnbundle
| GroupUpdateKeys
| GroupUpdatePath;
