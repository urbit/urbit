interface GroupWorkspace {
  type: 'group';
  group: string;
}

interface HomeWorkspace {
  type: 'home'
}

interface Messages {
  type: 'messages'
}

interface UqbarHome {
  type: 'uqbar-home'
}

export type Workspace = HomeWorkspace | GroupWorkspace | Messages | UqbarHome;
