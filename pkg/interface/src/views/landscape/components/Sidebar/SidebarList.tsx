import React, { ReactElement, useCallback } from 'react';
import { Associations, Graph, Unreads } from '@urbit/api';
import { patp, patp2dec } from 'urbit-ob';
import _ from 'lodash';

import { SidebarAssociationItem, SidebarDmItem } from './SidebarItem';
import useGraphState, { useInbox } from '~/logic/state/graph';
import useHarkState from '~/logic/state/hark';
import { alphabeticalOrder, getResourcePath, modulo } from '~/logic/lib/util';
import { SidebarListConfig, SidebarSort } from './types';
import { Workspace } from '~/types/workspace';
import useMetadataState from '~/logic/state/metadata';
import { useHistory } from 'react-router';
import { useShortcut } from '~/logic/state/settings';

function sidebarSort(unreads: Unreads, pending: Set<string>): Record<SidebarSort, (a: string, b: string) => number> {
  const { associations } = useMetadataState.getState();
  const alphabetical = (a: string, b: string) => {
    const aAssoc = associations[a];
    const bAssoc = associations[b];
    const aTitle = aAssoc?.metadata?.title || a;
    const bTitle = bAssoc?.metadata?.title || b;

    return alphabeticalOrder(aTitle, bTitle);
  };

  const lastUpdated = (a: string, b: string) => {
    const aPend = pending.has(a.slice(1));
    const bPend = pending.has(b.slice(1));
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

function getItems(associations: Associations, workspace: Workspace, inbox: Graph, pending: Set<string>) {
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
  const pend = workspace.type !== 'messages'
    ? []
    : Array.from(pending).map(s => `~${s}`);

  return [...filtered, ..._.union(direct, pend)];
}

export function SidebarList(props: {
  config: SidebarListConfig;
  baseUrl: string;
  group?: string;
  selected?: string;
  workspace: Workspace;
}): ReactElement {
  const { selected, config, workspace } = props;
  const associations = useMetadataState(state => state.associations);
  const inbox = useInbox();
  const graphKeys = useGraphState(s => s.graphKeys);
  const pending = useGraphState(s => s.pendingDms);
  const unreads = useHarkState(s => s.unreads);

  const ordered = getItems(associations, workspace, inbox, pending)
    .sort(sidebarSort(unreads, pending)[config.sortBy]);

  const history = useHistory();

  const cycleChannels = useCallback((backward: boolean) => {
    const idx = ordered.findIndex(s => s === selected);
    const offset = backward ? -1 : 1;

    const newIdx = modulo(idx+offset, ordered.length - 1);
    const newChannel = ordered[newIdx];
    let path = '';
    if(newChannel.startsWith('~')) {
      path = `/~landscape/messages/dm/${newChannel}`;
    } else {
      const { metadata, resource } = associations.graph[ordered[newIdx]];
      const joined = graphKeys.has(resource.slice(7));
      if ('graph' in metadata.config) {
        path = getResourcePath(workspace, resource, joined, metadata.config.graph);
      }
    }
    history.push(path);
  }, [ordered, selected, history.push]);

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
            selected={pathOrShip === selected}
            association={associations.graph[pathOrShip]}
            hideUnjoined={config.hideUnjoined}
            workspace={workspace}
          />
          ) : (
            <SidebarDmItem
              key={pathOrShip}
              ship={pathOrShip}
              workspace={workspace}
              selected={pathOrShip === selected}
              pending={pending.has(pathOrShip.slice(1))}
            />

          );
      })}
    </>
  );
}
