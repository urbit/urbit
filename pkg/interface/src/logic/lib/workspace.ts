import { Associations } from '@urbit/api';
import { Workspace } from '~/types';

export function getTitleFromWorkspace(
  associations: Associations,
  workspace: Workspace
) {
  switch (workspace.type) {
    case 'home':
      return 'My Channels';
    case 'uqbar-home':
      return 'Home';
    case 'messages':
      return 'Messages';
    case 'group':
      const association = associations.groups[workspace.group];
      return association?.metadata?.title || '';
  }
}

export function getGroupFromWorkspace(
  workspace: Workspace
): string {
  if (workspace.type === 'group') {
    return workspace.group;
  }

  return '';
}
