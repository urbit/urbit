import React, { ReactElement, useCallback, useState } from 'react';
import { Associations, Graph, Unreads } from '@urbit/api';
import { patp, patp2dec } from 'urbit-ob';
import _, { keyBy } from 'lodash';

import { SidebarAssociationItem, SidebarDmItem, SidebarItemBase, SidebarPendingItem } from './SidebarItem';
import useGraphState, { useInbox } from '~/logic/state/graph';
import useHarkState from '~/logic/state/hark';
import { alphabeticalOrder, getResourcePath, modulo } from '~/logic/lib/util';
import { SidebarListConfig, SidebarSort } from './types';
import { Workspace } from '~/types/workspace';
import useMetadataState from '~/logic/state/metadata';
import { useHistory } from 'react-router';
import { useShortcut } from '~/logic/state/settings';
import useGroupState from '~/logic/state/group';
import useInviteState from '~/logic/state/invite';
import { getGraphUnreads, sortGroupsAlph } from '~/views/apps/launch/components/Groups';
import { Box, Icon, LoadingSpinner } from '@tlon/indigo-react';

function dmUnreads(unreads) {
  let unreadCount = 0;
  for (const key in unreads) {
    if (key.includes('dm-inbox')) {
      unreadCount += unreads[key]?.count || 0;
    }
  }
  return unreadCount;
}

function sidebarSort(unreads: Unreads, pending: string[]): Record<SidebarSort, (a: string, b: string) => number> {
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

function getItems(associations: Associations, workspace: Workspace, inbox: Graph, pending: string[]) {
   const filtered = Object.keys(associations.graph).filter((a) => {
     const assoc = associations.graph[a];
     if(!('graph' in assoc.metadata.config)) {
       return false;
    }
      if (workspace?.type === 'group') {
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
    : pending

  return _.union(direct, pend, filtered);
}

function SidebarGroup({ baseUrl, selected, config, workspace, title }: {
  config: SidebarListConfig;
  baseUrl: string;
  selected?: string;
  title?: string;
  workspace: Workspace;
}): ReactElement {
  const groupSelected = workspace.type === 'messages' || workspace.type === 'group' && baseUrl.includes(workspace.group);

  const [collapsed, setCollapsed] = useState(!groupSelected);
  const associations = useMetadataState(state => state.associations);
  const groups = useGroupState(s => s.groups);
  const inbox = useInbox();
  const graphKeys = useGraphState(s => s.graphKeys);
  const pendingDms = useGraphState(s => [...s.pendingDms].map(s => `~${s}`));
  const pendingGroupChats = useGroupState(s => _.pickBy(s.pendingJoin, (req, rid) => !(rid in groups) && req.app === 'graph'));
  const inviteGroupChats = useInviteState(
    s => Object.values(s.invites?.['graph'] || {})
    .map(inv => `/ship/~${inv.resource.ship}/${inv.resource.name}`).filter(group => !(group in groups))
  );
  const pending = [...pendingDms, ...Object.keys(pendingGroupChats), ...inviteGroupChats];
  const { unreads, unseen } = useHarkState();

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
      const association = associations.graph[ordered[newIdx]];
      if(!association) {
        path = '/~landscape/messages';
        return;
      } else {
        const { metadata, resource } = association;
        const joined = graphKeys.has(resource.slice(7));
        if ('graph' in metadata.config) {
          path = getResourcePath(workspace, resource, joined, metadata.config.graph);
        }
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

  let hasNotification = false;
  if (workspace.type === 'group') {
    for (const key in unseen) {
      const formattedKey = key.replace('landscape/graph', '/ship').replace('/mention', '');
      if (associations.graph[formattedKey]?.group === workspace?.group) {
        console.log(1, associations.graph[formattedKey]?.group, workspace?.group)
        hasNotification = true;
        break;
      }
    }
  }
  const graphUnreads = getGraphUnreads(associations || ({} as Associations));
  const path = workspace.type === 'group' ? workspace.group : '';
  const unreadCount = workspace.type === 'group' ? graphUnreads(path) : dmUnreads(unreads);
  const hasUnread = unreadCount > 0;
  const isSynced = true;
  const isPending = false;
  const to = `/~landscape${workspace.type === 'group' ? workspace?.group : '/messages'}`;
  // TODO: scroll until group is at the top of the sidebar

  return (
    <Box>
      <SidebarItemBase
        to={to}
        selected={groupSelected}
        hasUnread={hasUnread}
        unreadCount={unreadCount}
        isSynced={isSynced}
        title={title || 'Messages'}
        hasNotification={hasNotification}
        pending={isPending}
        onClick={() => setCollapsed(false)}
      >
        {workspace?.type !== 'messages' && (
          <Icon
            p={1}
            pr="0"
            display="block"
            onClick={(e) => {
              e.preventDefault();
              e.stopPropagation();
              setCollapsed(!collapsed);
            }}
            icon={collapsed ? 'TriangleEast' : 'TriangleSouth'}
          />
        )}
      </SidebarItemBase>
      {!collapsed && (
        <Box>
          {ordered.map((pathOrShip) => {
            const pathAsGraph = pathOrShip.replace('ship', 'graph');
            const { count, each } = unreads[pathAsGraph] || { count: 0, each: [] };

            return pathOrShip.startsWith('~') ? (
                <SidebarDmItem
                  key={pathOrShip}
                  ship={pathOrShip}
                  workspace={workspace}
                  selected={pathOrShip === selected}
                  pending={pending.includes(pathOrShip)}
                  indent={1}
                />
              ) : pending.includes(pathOrShip) ? (
                <SidebarPendingItem
                  key={pathOrShip}
                  path={pathOrShip}
                  selected={pathOrShip === selected}
                  indent={1}
                />
              ) : (
              <SidebarAssociationItem
                key={pathOrShip}
                selected={pathOrShip === selected}
                association={associations.graph[pathOrShip]}
                hideUnjoined={config.hideUnjoined}
                workspace={workspace}
                unreadCount={count + each.length}
                hasNotification={unseen?.[`landscape${pathAsGraph}/mention`]}
                indent={1}
              />
              );
          })}
        </Box>
      )}
    </Box>
  );
}

export function SidebarGroupList({
  messages = false,
  ...props
}: {
  config: SidebarListConfig;
  baseUrl: string;
  selected?: string;
  messages?: boolean;
}): ReactElement {
  const associations = useMetadataState(state => state.associations);
  const groups = useGroupState(s => s.groups);

  const groupList = Object.values(associations?.groups || {})
    .filter(e => e?.group in groups)
    .sort(sortGroupsAlph);

  return (
    messages ? (
      <SidebarGroup
        {...props}
        workspace={{ type: 'messages' }}
      />
    )
    : !groupList?.length
    ? (
      <Box width="100%" height="100%" display="flex" alignItems="center" justifyContent="center">
        <LoadingSpinner />
      </Box>
    )
    : (
      <>
        {groupList.map((g) => {
          return (
            <SidebarGroup
              key={g.group}
              {...props}
              workspace={{ type: 'group', group: g.group }}
              title={g.metadata.title}
            />
          );
        })}
      </>
    )
  );
}
