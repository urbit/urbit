import {
  Box,
    Col
} from '@tlon/indigo-react';
import React, { ReactElement } from 'react';
import styled from 'styled-components';
import { roleForShip } from '~/logic/lib/group';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import { getGroupFromWorkspace } from '~/logic/lib/workspace';
import useGroupState from '~/logic/state/group';
import { Workspace } from '~/types';
import { GroupSwitcher } from '../GroupSwitcher';
import { SidebarGroupList } from './SidebarGroupList';
import { SidebarListConfig } from './types';

const ScrollbarLessCol = styled(Col)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;

interface SidebarProps {
  recentGroups: string[];
  selected?: string;
  baseUrl: string;
  workspace: Workspace;
}

export function Sidebar(props: SidebarProps): ReactElement | null {
  const { selected, workspace } = props;
  const groupPath = getGroupFromWorkspace(workspace);

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
  const focusMessages = props.baseUrl.includes('~landscape/messages');

  return (
    <Box>
      <GroupSwitcher
        recentGroups={props.recentGroups}
        baseUrl={props.baseUrl}
        isAdmin={isAdmin}
        workspace={props.workspace}
      />
      <ScrollbarLessCol
        display="flex"
        width="100%"
        gridRow="1/2"
        gridColumn="1/2"
        borderTopLeftRadius={2}
        borderRight={1}
        borderRightColor="lightGray"
        overflowY="scroll"
        fontSize={0}
        position="relative"
        height={focusMessages ? 'calc(50% - 24px)' : 'calc(75% - 23px)'}
        borderBottom={1}
        borderBottomColor="lightGray"
        pb={1}
      >
        <Box mt={2} />
        <SidebarGroupList
          config={config}
          selected={selected}
          baseUrl={props.baseUrl}
        />
      </ScrollbarLessCol>
      <ScrollbarLessCol
        display="flex"
        width="100%"
        gridRow="1/2"
        gridColumn="1/2"
        borderTopLeftRadius={2}
        borderRight={1}
        borderRightColor="lightGray"
        overflowY="scroll"
        fontSize={0}
        position="relative"
        height={focusMessages ? 'calc(50% - 23px)' : 'calc(25% - 24px)'}
      >
        <SidebarGroupList
          config={config}
          selected={selected}
          baseUrl={props.baseUrl}
          messages
        />
      </ScrollbarLessCol>
    </Box>
  );
}
