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

const ScrollbarLessCol = styled(Col)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;

interface SidebarProps {
  contacts: Rolodex;
  children: ReactNode;
  recentGroups: string[];
  invites: Invites ;
  api: GlobalApi;
  associations: Associations;
  selected?: string;
  selectedGroup?: string;
  includeUnmanaged?: boolean;
  groups: Groups;
  apps: SidebarAppConfigs;
  baseUrl: string;
  mobileHide?: boolean;
  workspace: Workspace;
}

export function Sidebar(props: SidebarProps): ReactElement {
  const { associations, selected, workspace } = props;
  const groupPath = getGroupFromWorkspace(workspace);
  const display = props.mobileHide ? ['none', 'flex'] : 'flex';
  if (!associations) {
    return null;
  }

  const [config, setConfig] = useLocalStorageState<SidebarListConfig>(
    `group-config:${groupPath || 'home'}`,
    {
      sortBy: 'lastUpdated',
      hideUnjoined: false
    }
  );

  const role = props.groups?.[groupPath] ? roleForShip(props.groups[groupPath], window.ship) : undefined;
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
        associations={associations}
        recentGroups={props.recentGroups}
        baseUrl={props.baseUrl}
        isAdmin={isAdmin}
        workspace={props.workspace}
      />
      <SidebarListHeader
        associations={associations}
        contacts={props.contacts}
        baseUrl={props.baseUrl}
        groups={props.groups}
        initialValues={config}
        handleSubmit={setConfig}
        selected={selected || ''}
        workspace={workspace}
        api={props.api}
        history={props.history}
      />
      <SidebarList
        config={config}
        associations={associations}
        selected={selected}
        group={groupPath}
        groups={props.groups}
        apps={props.apps}
        baseUrl={props.baseUrl}
        workspace={workspace}
        contacts={props.contacts}
      />
    </ScrollbarLessCol>
  );
}
