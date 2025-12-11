import { Associations } from '@urbit/api';
import { Workspace } from '~/types';

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
      return associations.groups[workspace.group]?.metadata?.title || 'Groups';
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
