export type LocalUpdate =
  LocalUpdateSidebarToggle
| LocalUpdateSetDark
| LocalUpdateSetOmniboxShown
| LocalUpdateBaseHash;

interface LocalUpdateSidebarToggle {
  sidebarToggle: boolean;
}

interface LocalUpdateSetDark {
  setDark: boolean;
}

interface LocalUpdateBaseHash {
  baseHash: string;
}

interface LocalUpdateSetOmniboxShown {
  omniboxShown: boolean;
}
