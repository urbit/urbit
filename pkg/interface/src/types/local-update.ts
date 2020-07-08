import { Path } from './noun';

export type LocalUpdate =
  LocalUpdateSidebarToggle
| LocalUpdateSelectedGroups
| LocalUpdateSetDark;

interface LocalUpdateSidebarToggle {
  sidebarToggle: boolean;
}

interface LocalUpdateSelectedGroups {
  selected: SelectedGroup[];
}

interface LocalUpdateSetDark {
  setDark: boolean;
}

export type SelectedGroup = [Path, string];
