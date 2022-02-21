import React, { MouseEvent, ReactElement, useCallback, useEffect, useMemo, useState } from 'react';
import { resourceAsPath } from '@urbit/api';
import _, { find } from 'lodash';
import { DragDropContext } from 'react-beautiful-dnd';

import { SidebarItemBase } from './SidebarItem';
import { SidebarListConfig } from './types';
import useMetadataState, { usePreview } from '~/logic/state/metadata';
import { useHistory } from 'react-router';
import useSettingsState from '~/logic/state/settings';
import useGroupState from '~/logic/state/group';
import useInviteState from '~/logic/state/invite';
import { sortGroupsAlph } from '~/views/apps/launch/components/Groups';
import { Box, LoadingSpinner } from '@tlon/indigo-react';
import { useQuery } from '~/logic/lib/useQuery';
import { GroupOrder, SidebarGroupSorter } from './SidebarGroupSorter';
import { SidebarGroup } from './SidebarGroup';

interface PendingSidebarGroupProps {
  path: string;
}

function PendingSidebarGroup({ path }: PendingSidebarGroupProps) {
  const history = useHistory();
  const { preview, error } = usePreview(path);
  const title = preview?.metadata?.title || path;
  const { toQuery } = useQuery();
  const onClick = (e: MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    history.push(toQuery({ 'join-kind': 'groups', 'join-path': path }));
  };

  const joining = useGroupState(s => s.pendingJoin[path]?.progress);
  const isJoining = Boolean(joining && joining !== 'done');

  return (
    <SidebarItemBase
      to="/"
      onClick={onClick}
      title={title}
      selected={false}
      pending={isJoining}
      hasUnread={false}
      hasNotification={!joining}
      isSynced={!joining}
      isGroup
    />
  );
}

export function SidebarGroupList({
  messages = false,
  changingSort = false,
  ...props
}: {
  config: SidebarListConfig;
  baseUrl: string;
  changingSort?: boolean;
  selected?: string;
  messages?: boolean;
}): ReactElement {
  const { associations } = useMetadataState();
  const { groups } = useGroupState();
  const { groupSorter, putEntry } = useSettingsState.getState();
  const [groupOrder, setGroupOrder] = useState<GroupOrder>(JSON.parse(groupSorter.order || '[]'));
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    setTimeout(() => setLoading(false), 8000);
  }, []);

  const saveGroupOrder = useCallback((newOrder) => {
    putEntry('groupSorter', 'order', JSON.stringify(newOrder));
    setGroupOrder(newOrder);
  }, [putEntry, setGroupOrder]);

  const handleDragAndDrop = useCallback(({ source, destination }) => {
    if (!destination) {
      return;
    }
    // TODO: figure out how to drag onto folders
    const items = Array.from(groupOrder);
    const [reorderedItem] = items.splice(source.index, 1);
    items.splice(destination.index, 0, reorderedItem);
    saveGroupOrder(items);
  }, [groupOrder, saveGroupOrder]);

  const groupList = useMemo(() => Object.values(associations?.groups || {})
    .filter(e => e?.group in groups)
    .sort(sortGroupsAlph), [associations, groups]);

  useEffect(() => {
    const newGroupOrder = JSON.parse(groupSorter.order || '[]');
    if (newGroupOrder.length) {
      setGroupOrder(newGroupOrder);
    }
  }, [groupSorter]);

  useEffect(() => {
    if (!groupOrder.length) {
      return;
    }
    // add groups to groupSorter.order if they're missing (as in, recently joined)
    let sortedGroups = [];
    for (const key in groupOrder) {
      if (typeof groupOrder[key] === 'string' && groupList.find(e => e?.group === groupOrder[key])) {
        sortedGroups.push(groupOrder[key]);
      } else if (!(typeof groupOrder[key] === 'string') && groupOrder[key]?.groups) {
        sortedGroups = sortedGroups.concat(groupOrder[key]?.groups);
      }
    }
    const missingGroups = groupList.map(({ group }) => group).filter(g => !sortedGroups.includes(g));
    if (missingGroups.length) {
      saveGroupOrder(groupOrder.concat(missingGroups));
    }
  }, [groupList]);

  const joining = useGroupState(s =>
    _.omit(
      _.pickBy(s.pendingJoin || {}, req => req.app === 'groups' && req.progress != 'abort'),
      groupList.map(g => g.group)
    )
  );

  const invites = useInviteState(s => Object.values(s.invites?.['groups'] || {}).map(inv => resourceAsPath(inv?.resource)) || []);
  const pending = _.union(invites, Object.keys(joining)).filter(group =>
    !(group in (groups?.groups || {})) && !(group in (associations.groups || {}))
  );

  if (messages) {
    return <SidebarGroup {...props} workspace={{ type: 'messages' }} />;
  } else if (!groupList.length && loading) {
    return <Box width="100%" height="100%" display="flex" alignItems="center" justifyContent="center">
      <LoadingSpinner />
    </Box>;
  }

  if (changingSort) {
    const groupsToSort = groupOrder.length ? groupOrder : groupList.map(g => g.group);
    return <DragDropContext onDragEnd={handleDragAndDrop}>
      <SidebarGroupSorter groupOrder={groupsToSort} />
    </DragDropContext>;
  }

  return (
    <>
      <SidebarGroup {...props} workspace={{ type: 'home' }} />
      {groupOrder.length ? groupOrder.map((go) => {
        if (typeof go === 'string') {
          const g = associations.groups[go];
          if (!g) {
            return null;
          }

          return (
            <SidebarGroup
              key={g.group}
              {...props}
              workspace={{ type: 'group', group: g.group }}
              title={g.metadata.title}
            />
          );
        }

        // TODO: handle folders in groupOrder
        return null;
      }) : (
        groupList.map((g: any) => <SidebarGroup
          key={g.group} {...props}
          workspace={{ type: 'group', group: g.group }}
          title={g.metadata.title}
        />)
      )}
      {pending.map(p => <PendingSidebarGroup key={p} path={p} />)}
    </>
  );
}
