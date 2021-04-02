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

export type Workspace = HomeWorkspace | GroupWorkspace | Messages;
