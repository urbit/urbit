import { Path } from './noun';

export type LocalUpdate =
  LocalUpdateSidebarToggle
| LocalUpdateSelectedGroups
| LocalUpdateBaseHash;

interface LocalUpdateSidebarToggle {
  sidebarToggle: boolean;
}

interface LocalUpdateSelectedGroups {
  selected: SelectedGroup[];
}

interface LocalUpdateBaseHash {
  baseHash: string;
}

export type SelectedGroup = [Path, string];
