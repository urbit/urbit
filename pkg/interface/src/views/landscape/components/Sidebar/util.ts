import { Associations, Graph, Unreads } from '@urbit/api';
import { patp, patp2dec } from 'urbit-ob';
import _ from 'lodash';

import { alphabeticalOrder } from '~/logic/lib/util';
import useMetadataState from '~/logic/state/metadata';
import { Workspace } from '~/types';
import { SidebarSort } from './types';

export function dmUnreads(unreads) {
  let unreadCount = 0;
  for (const key in unreads) {
    if (key.includes('dm-inbox')) {
      unreadCount += unreads[key]?.count || 0;
    }
  }
  return unreadCount;
}

export function getItems(associations: Associations, workspace: Workspace, inbox: Graph, pending: string[]) {
  const filtered = Object.keys(associations.graph).filter((a) => {
    const assoc = associations.graph[a];
    if (!('graph' in assoc.metadata.config)) {
      return false;
   } else if (workspace?.type === 'group') {
     const group = workspace.group;
     return group ? (
       assoc.group === group &&
       !assoc.metadata.hidden
     ) : (
       !(assoc.group in associations.groups) &&
       'graph' in assoc.metadata.config &&
       assoc.metadata.config.graph !== 'chat' &&
       !assoc.metadata.hidden
     );
   } else if (workspace?.type === 'messages') {
     return (
       !assoc.metadata.hidden &&
       !(assoc.group in associations.groups) &&
       assoc.metadata.config.graph === 'chat'
     );
   } else {
     return (
       !(assoc.group in associations.groups) &&
       assoc.metadata.config.graph !== 'chat'
     );
   }
  });
 const direct: string[] = workspace.type !== 'messages' ? []
   : inbox.keys().map(x => patp(x.toString()));
 const pend = workspace.type !== 'messages'
   ? []
   : pending;

 return _.union(direct, pend, filtered);
}

export function sidebarSort(unreads: Unreads, pending: string[]): Record<SidebarSort, (a: string, b: string) => number> {
  const { associations } = useMetadataState.getState();
  const alphabetical = (a: string, b: string) => {
    const aAssoc = associations[a];
    const bAssoc = associations[b];
    const aTitle = aAssoc?.metadata?.title || a;
    const bTitle = bAssoc?.metadata?.title || b;

    return alphabeticalOrder(aTitle, bTitle);
  };

  const lastUpdated = (a: string, b: string) => {
    const aPend = pending.includes(a);
    const bPend = pending.includes(b);
    if(aPend && !bPend) {
      return -1;
    }
    if(bPend && !aPend) {
      return 1;
    }
    const aUpdated = a.startsWith('~')
      ?  (unreads?.[`/graph/~${window.ship}/dm-inbox/${patp2dec(a)}`]?.last || 0)
      :  (unreads?.[`/graph/${a.slice(6)}`]?.last || 0);

    const bUpdated = b.startsWith('~')
      ?  (unreads?.[`/graph/~${window.ship}/dm-inbox/${patp2dec(b)}`]?.last || 0)
      :  (unreads?.[`/graph/${b.slice(6)}`]?.last || 0);

    return bUpdated - aUpdated || alphabetical(a, b);
  };

  return {
    asc: alphabetical,
    lastUpdated
  };
}
