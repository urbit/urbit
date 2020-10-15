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

interface LocalUpdateSetOmniboxShown {
  omniboxShown: boolean;
}

export interface LocalUpdateRemoteContentPolicy {
  imageShown: boolean;
  audioShown: boolean;
  videoShown: boolean;
  oembedShown: boolean;
}

interface BackgroundConfigUrl {
  type: 'url';
  url: string;
}

interface BackgroundConfigColor {
  type: 'color';
  color: string;
}

export type BackgroundConfig = BackgroundConfigUrl | BackgroundConfigColor | undefined;

export type LocalUpdate =
  LocalUpdateSidebarToggle
| LocalUpdateSetDark
| LocalUpdateBaseHash
| LocalUpdateBackgroundConfig
| LocalUpdateHideAvatars
| LocalUpdateHideNicknames
| LocalUpdateSetOmniboxShown
| LocalUpdateRemoteContentPolicy;