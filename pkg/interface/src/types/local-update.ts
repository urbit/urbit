import { Path } from './noun';

export type LocalUpdate =
  LocalUpdateSidebarToggle
| LocalUpdateSelectedGroups
| LocalUpdateSetDark
| LocalUpdateBaseHash
| LocalUpdateBackgroundConfig
| LocalUpdateHideAvatars
| LocalUpdateHideNicknames;

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

interface LocalUpdateBackgroundConfig {
  backgroundConfig: BackgroundConfig;
}

interface LocalUpdateHideAvatars {
  hideAvatars: boolean;
}

interface LocalUpdateHideNicknames {
  hideNicknames: boolean;
}

export type BackgroundConfig = BackgroundConfigUrl | BackgroundConfigColor | undefined;

interface BackgroundConfigUrl {
  type: 'url';
  url: string;
}

interface BackgroundConfigColor {
  type: 'color';
  color: string;
}


export type SelectedGroup = [Path, string];
