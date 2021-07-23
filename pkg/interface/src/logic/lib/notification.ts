import { GraphNotifIndex, GraphNotificationContents } from '@urbit/api';

export function getParentIndex(
  idx: GraphNotifIndex,
  contents: GraphNotificationContents
) {
  const origIndex = contents[0].index.slice(1).split('/');
  const ret = (i: string[]) => `/${i.join('/')}`;
  switch (idx.description) {
    case 'link':
      return '/';
    case 'comment':
      return ret(origIndex.slice(0, 1));
    case 'note':
      return '/';
    case 'mention':
      return undefined;
    default:
      return undefined;
  }
}
