import React, { ReactElement, ReactNode, useRef } from 'react';
import styled from 'styled-components';
import {
  Col
} from '@tlon/indigo-react';

import GlobalApi from '~/logic/api/global';
import { GroupSwitcher } from '../GroupSwitcher';
import {
  Associations,
  Workspace,
  Groups,
  Invites,
  Rolodex
} from '@urbit/api';
import { SidebarListHeader } from './SidebarListHeader';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import { getGroupFromWorkspace } from '~/logic/lib/workspace';
import { SidebarAppConfigs } from './types';
import { SidebarList } from './SidebarList';
import { roleForShip } from '~/logic/lib/group';
import { useTutorialModal } from '~/views/components/useTutorialModal';
import useGroupState from '~/logic/state/group';
import useMetadataState from '~/logic/state/metadata';

const ScrollbarLessCol = styled(Col)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;

interface SidebarProps {
  children: ReactNode;
  recentGroups: string[];
  api: GlobalApi;
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

  const anchorRef = useRef<HTMLElement | null>(null);
  useTutorialModal('channels', true, anchorRef);

  return (
    <ScrollbarLessCol
      ref={anchorRef}
      display={display}
      width="100%"
      gridRow="1/2"
      gridColumn="1/2"
      borderTopLeftRadius='2'
      borderRight={1}
      borderRightColor="washedGray"
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
        api={props.api}
        history={props.history}
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
