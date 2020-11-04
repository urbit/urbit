

interface GroupWorkspace {
  type: 'group';
  group: string;
}

interface HomeWorkspace {
  type: 'home'
}

export type Workspace = HomeWorkspace | GroupWorkspace;
