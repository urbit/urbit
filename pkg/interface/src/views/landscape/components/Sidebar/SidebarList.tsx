import React, { ReactElement } from 'react';
import { Associations, AppAssociations, Groups, Rolodex, Graph, UnreadStats } from '@urbit/api';
import { patp, patp2dec } from 'urbit-ob';

import { alphabeticalOrder } from '~/logic/lib/util';
import { SidebarAppConfigs, SidebarListConfig, SidebarSort } from './types';
import { SidebarAssociationItem, SidebarDmItem } from './SidebarItem';
import { Workspace } from '~/types/workspace';
import useMetadataState from '~/logic/state/metadata';
import {useInbox} from '~/logic/state/graph';
import useHarkState from '~/logic/state/hark';

function sidebarSort(
  associations: AppAssociations,
  apps: SidebarAppConfigs,
  inboxUnreads: Record<string, UnreadStats>
): Record<SidebarSort, (a: string, b: string) => number> {
  const alphabetical = (a: string, b: string) => {
    const aAssoc = associations[a];
    const bAssoc = associations[b];
    const aTitle = aAssoc?.metadata?.title || b;
    const bTitle = bAssoc?.metadata?.title || b;

    return alphabeticalOrder(aTitle, bTitle);
  };

  const lastUpdated = (a: string, b: string) => {
    const aAssoc = associations[a];
    const bAssoc = associations[b];
    const aAppName = aAssoc?.['app-name'];
    const bAppName = bAssoc?.['app-name'];

    const aUpdated = a.startsWith('~') 
      ?  (inboxUnreads?.[`/${patp2dec(a)}`]?.last)
      :  apps[aAppName]?.lastUpdated(a) || 0;
    const bUpdated = b.startsWith('~')
      ?  (inboxUnreads?.[`/${patp2dec(b)}`]?.last || 0)
      :  apps[bAppName]?.lastUpdated(b) || 0;

    return bUpdated - aUpdated || alphabetical(a, b);
  };

  return {
    asc: alphabetical,
    lastUpdated
  };
}


function getItems(associations: Associations, workspace: Workspace, inbox: Graph) {
   const filtered = Object.keys(associations.graph).filter((a) => {
      const assoc = associations.graph[a];
      if (workspace?.type === 'messages') {
        return (
          !(assoc.group in associations.groups) &&
          'graph' in assoc.metadata.config &&
          assoc.metadata.config.graph === 'chat'
        );
      } else {
        return group ? (
          assoc.group === group &&
          !assoc.metadata.hidden
        ) : (
          !(assoc.group in associationState.groups) &&
          'graph' in assoc.metadata.config &&
          assoc.metadata.config.graph !== 'chat' &&
          !assoc.metadata.hidden
        );
      }
   });
  const direct: string[] = workspace.type !== 'messages' ? []
    : inbox.keys().map(x => patp(x.toJSNumber()))

  return [...filtered, ...direct];

}

export function SidebarList(props: {
  apps: SidebarAppConfigs;
  config: SidebarListConfig;
  baseUrl: string;
  group?: string;
  selected?: string;
  workspace: Workspace;
}): ReactElement {
  const { selected, group, config, workspace } = props;
  const associations = useMetadataState(state => state.associations);
  const inbox = useInbox();
  const unreads = useHarkState(s => s.unreads.graph?.[`/ship/~${window.ship}/inbox`]);


  const ordered = getItems(associations, workspace, inbox)
    .sort(sidebarSort(associations, props.apps, unreads)[config.sortBy]);

  return (
    <>
      {ordered.map((pathOrShip) => {
        
        return pathOrShip.startsWith('/') ? (
          <SidebarAssociationItem
            key={pathOrShip}
            path={pathOrShip}
            selected={pathOrShip === selected}
            association={associations[pathOrShip]}
            apps={props.apps}
            hideUnjoined={config.hideUnjoined}
            workspace={workspace}
          />
          ) : (
            <SidebarDmItem
              key={pathOrShip}
              ship={pathOrShip}
              workspace={workspace}
              selected={pathOrShip === selected}
            />

          );
      })}
    </>
  );
}
