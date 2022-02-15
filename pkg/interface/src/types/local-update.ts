export const leapCategories = ['mychannel', 'messages', 'updates', 'profile', 'settings', 'logout'];

export type LeapCategories = typeof leapCategories[number];

interface LocalUpdateSetDark {
  setDark: boolean;
}

interface LocalUpdateBaseHash {
  baseHash: string;
}

interface LocalUpdateRuntimeLag {
  runtimeLag: boolean;
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

export interface RemoteContentPolicy {
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
| LocalUpdateSetDark
| LocalUpdateBaseHash
| LocalUpdateRuntimeLag
| LocalUpdateBackgroundConfig
| LocalUpdateHideAvatars
| LocalUpdateHideNicknames
| LocalUpdateSetOmniboxShown
| RemoteContentPolicy;
