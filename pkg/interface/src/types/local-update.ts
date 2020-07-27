import { Path } from './noun';

export type LocalUpdate =
  LocalUpdateSidebarToggle
| LocalUpdateSelectedGroups
| LocalUpdateSetDark
| LocalUpdateBaseHash;

interface LocalUpdateSidebarToggle {
  sidebarToggle: boolean;
}

interface LocalUpdateSelectedGroups {
  selected: SelectedGroup[];
}

interface LocalUpdateSetDark {
  setDark: boolean;
}

interface LocalUpdateBaseHash {
  baseHash: string;
}

export type SelectedGroup = [Path, string];
