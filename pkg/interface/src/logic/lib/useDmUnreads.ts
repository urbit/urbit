import { useMemo } from 'react';
import useGraphState, { useInbox } from '../state/graph';
import useHarkState from '../state/hark';
import useMetadataState from '../state/metadata';
import _ from 'lodash';

export function useDmUnreads() {
  const { unreads } = useHarkState();
  const inbox = useInbox();
  const pendingDms = useGraphState(s => [...s.pendingDms].map(s => `~${s}`));
  const associations = useMetadataState(s => s.associations);

  const unreadDmCount = useMemo(() => {
    const dmGroups = Object.keys(associations.graph).filter((a) => {
      const assoc = associations.graph[a];
      return ('graph' in assoc.metadata.config) && (
        !assoc.metadata.hidden &&
        !(assoc.group in associations.groups) &&
        assoc.metadata.config.graph === 'chat'
      );
    });

    const dms = inbox.keys().map(x => `/graph/~${window.ship}/dm-inbox/${x}`);
    const allDms = _.union(dmGroups, pendingDms, dms);

    return allDms.reduce((total, dmPath) => {
      const pathAsGraph = dmPath.replace('ship', 'graph');
      const { count, each } = unreads[pathAsGraph] || { count: 0, each: [] };
      return total + count + each.length;
    }, 0);
  }, [inbox, pendingDms, associations.graph]);

  return {
    unreadDmCount
  };
}
