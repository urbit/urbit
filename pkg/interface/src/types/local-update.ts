export type LocalUpdate =
  LocalUpdateSidebarToggle
| LocalUpdateSetDark
| LocalUpdateBaseHash
| LocalUpdateBackgroundConfig
| LocalUpdateHideAvatars
| LocalUpdateHideNicknames
| LocalUpdateSetOmniboxShown;

interface LocalUpdateSidebarToggle {
  sidebarToggle: boolean;
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

interface LocalUpdateSetOmniboxShown {
  omniboxShown: boolean;
}
