import {
    Col
} from '@tlon/indigo-react';
import React, { ReactElement, useRef } from 'react';
import styled from 'styled-components';
import { roleForShip } from '~/logic/lib/group';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import { getGroupFromWorkspace } from '~/logic/lib/workspace';
import useGroupState from '~/logic/state/group';
import { Workspace } from '~/types';
import { useTutorialModal } from '~/views/components/useTutorialModal';
import { GroupSwitcher } from '../GroupSwitcher';
import { SidebarList } from './SidebarList';
import { SidebarListHeader } from './SidebarListHeader';
import { SidebarAppConfigs, SidebarListConfig } from './types';

const ScrollbarLessCol = styled(Col)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;

interface SidebarProps {
  recentGroups: string[];
  selected?: string;
  selectedGroup?: string;
  apps: SidebarAppConfigs;
  baseUrl: string;
  mobileHide?: boolean;
  workspace: Workspace;
}

export function Sidebar(props: SidebarProps): ReactElement | null {
  const { selected, workspace } = props;
  const groupPath = getGroupFromWorkspace(workspace);
  const display = props.mobileHide ? ['none', 'flex'] : 'flex';

  const [config, setConfig] = useLocalStorageState<SidebarListConfig>(
    `group-config:${groupPath || 'home'}`,
    {
      sortBy: 'lastUpdated',
      hideUnjoined: false
    }
  );

  const groups = useGroupState(state => state.groups);

  const role = groups?.[groupPath] ? roleForShip(groups[groupPath], window.ship) : undefined;
  const isAdmin = (role === 'admin') || (workspace?.type === 'home');

  const anchorRef = useRef<HTMLDivElement>(null);
  useTutorialModal('channels', true, anchorRef);

  return (
    <ScrollbarLessCol
      ref={anchorRef}
      display={display}
      width="100%"
      gridRow="1/2"
      gridColumn="1/2"
      borderTopLeftRadius={2}
      borderRight={1}
      borderRightColor="lightGray"
      overflowY="scroll"
      fontSize={0}
      position="relative"
    >
      <GroupSwitcher
        recentGroups={props.recentGroups}
        baseUrl={props.baseUrl}
        isAdmin={isAdmin}
        workspace={props.workspace}
      />
      <SidebarListHeader
        baseUrl={props.baseUrl}
        initialValues={config}
        handleSubmit={setConfig}
        selected={selected || ''}
        workspace={workspace}
      />
      <SidebarList
        config={config}
        selected={selected}
        group={groupPath}
        apps={props.apps}
        baseUrl={props.baseUrl}
        workspace={workspace}
      />
    </ScrollbarLessCol>
  );
}
