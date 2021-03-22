export const tutorialProgress = ['hidden', 'start', 'group-desc', 'channels', 'chat', 'link', 'publish', 'profile', 'leap', 'notifications', 'done', 'exit'] as const;

export const leapCategories = ["mychannel", "messages", "updates", "profile", "logout"] as const;

export type LeapCategories = typeof leapCategories[number];

export type TutorialProgress = typeof tutorialProgress[number];
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
| LocalUpdateBackgroundConfig
| LocalUpdateHideAvatars
| LocalUpdateHideNicknames
| LocalUpdateSetOmniboxShown
| RemoteContentPolicy;
