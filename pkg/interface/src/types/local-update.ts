
export type LocalUpdate =
  LocalUpdateSidebarToggle
| LocalUpdateSetDark
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
