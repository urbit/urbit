const defaultApps = [
  'chat',
  'term',
  'groups',
  'link',
  'publish',
] as const;

export type DefaultApp = typeof defaultApps[number];

export default defaultApps;
