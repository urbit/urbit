import { Associations, Workspace } from '@urbit/api';

export function getTitleFromWorkspace(
  associations: Associations,
  workspace: Workspace
) {
  switch (workspace.type) {
    case 'home':
      return 'My Channels';
    case 'messages':
      return 'Messages';
    case 'group':
      const association = associations.groups[workspace.group];
      return association?.metadata?.title || '';
  }
}

export function getGroupFromWorkspace(
  workspace: Workspace
): string | undefined {
  if (workspace.type === 'group') {
    return workspace.group;
  }

  return undefined;
}
