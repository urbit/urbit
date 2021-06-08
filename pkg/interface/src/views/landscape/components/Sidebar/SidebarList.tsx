import React, { ReactElement, useCallback } from 'react';
import { AppAssociations, Associations, Graph, UnreadStats } from '@urbit/api';
import { patp, patp2dec } from 'urbit-ob';

import { SidebarAssociationItem, SidebarDmItem } from './SidebarItem';
import useGraphState, { useInbox } from '~/logic/state/graph';
import useHarkState from '~/logic/state/hark';
import { alphabeticalOrder, getResourcePath, modulo } from '~/logic/lib/util';
import { SidebarAppConfigs, SidebarListConfig, SidebarSort } from './types';
import { Workspace } from '~/types/workspace';
import useMetadataState from '~/logic/state/metadata';
import { useHistory } from 'react-router';
import { useShortcut } from '~/logic/state/settings';

function sidebarSort(
  associations: AppAssociations,
  apps: SidebarAppConfigs,
  inboxUnreads: Record<string, UnreadStats>
): Record<SidebarSort, (a: string, b: string) => number> {
  const alphabetical = (a: string, b: string) => {
    const aAssoc = associations[a];
    const bAssoc = associations[b];
    const aTitle = aAssoc?.metadata?.title || a;
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
      :  (apps[aAppName]?.lastUpdated(a) || 0);
    const bUpdated = b.startsWith('~')
      ?  (inboxUnreads?.[`/${patp2dec(b)}`]?.last || 0)
      :  (apps[bAppName]?.lastUpdated(b) || 0);

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
     if(!('graph' in assoc.metadata.config)) {
       return false;
    }
      if (workspace?.type === 'messages') {
        return (
          !assoc.metadata.hidden &&
          !(assoc.group in associations.groups) &&
          assoc.metadata.config.graph === 'chat'
        );
      } else if (workspace?.type === 'home') {
        return (
          !(assoc.group in associations.groups) &&
          assoc.metadata.config.graph !== 'chat'
        );
      } else {
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
      }
   });
  const direct: string[] = workspace.type !== 'messages' ? []
    : inbox.keys().map(x => patp(x.toString()));

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
  const { selected, config, workspace } = props;
  const associations = useMetadataState(state => state.associations);
  const inbox = useInbox();
  const unreads = useHarkState(s => s.unreads.graph?.[`/ship/~${window.ship}/dm-inbox`]);
  const graphKeys = useGraphState(s => s.graphKeys);

  const ordered = getItems(associations, workspace, inbox)
    .sort(sidebarSort(associations.graph, props.apps, unreads)[config.sortBy]);

  const history = useHistory();

  const cycleChannels = useCallback((backward: boolean) => {
    const idx = ordered.findIndex(s => s === selected);
    const offset = backward ? -1 : 1;

    const newIdx = modulo(idx+offset, ordered.length - 1);
    const { metadata, resource } = associations[ordered[newIdx]];
    const joined = graphKeys.has(resource.slice(7));
    let path = '/~landscape/home';
    if ('graph' in metadata.config) {
      path = getResourcePath(workspace, resource, joined, metadata.config.graph);
    }
    history.push(path);
  }, [selected, history.push]);

  useShortcut('cycleForward', useCallback((e: KeyboardEvent) => {
    cycleChannels(false);
    e.preventDefault();
  }, [cycleChannels]));

  useShortcut('cycleBack', useCallback((e: KeyboardEvent) => {
    cycleChannels(true);
    e.preventDefault();
  }, [cycleChannels]));

  return (
    <>
      {ordered.map((pathOrShip) => {
        return pathOrShip.startsWith('/') ? (
          <SidebarAssociationItem
            key={pathOrShip}
            path={pathOrShip}
            selected={pathOrShip === selected}
            association={associations.graph[pathOrShip]}
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
