import { Path } from './noun';

export type LocalUpdate =
  LocalUpdateSidebarToggle
| LocalUpdateSelectedGroups;

interface LocalUpdateSidebarToggle {
  sidebarToggle: boolean;
}

interface LocalUpdateSelectedGroups {
  selected: SelectedGroup[];
}

export type SelectedGroup = [Path, string];
