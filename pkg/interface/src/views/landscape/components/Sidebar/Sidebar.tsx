import React, { ReactNode } from "react";
import styled from 'styled-components';
import {
  Box,
  Col,
} from "@tlon/indigo-react";
import { Link } from "react-router-dom";

import GlobalApi from "~/logic/api/global";
import { GroupSwitcher } from "../GroupSwitcher";
import {
  Associations,
  Workspace,
  Groups,
  Invites,
} from "~/types";
import { SidebarListHeader } from "./SidebarListHeader";
import { useLocalStorageState } from "~/logic/lib/useLocalStorageState";
import { getGroupFromWorkspace } from "~/logic/lib/workspace";
import { SidebarAppConfigs } from './types';
import {SidebarList} from "./SidebarList";

interface SidebarProps {
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

// Magic spacer that because firefox doesn't correctly calculate
// position: sticky on a flex child
// remove when https://bugzilla.mozilla.org/show_bug.cgi?id=1488080
// is fixed
const SidebarStickySpacer = styled(Box)`
  height: 0px;
  flex-grow: 1;
  @-moz-document url-prefix() {
    & {
      height: ${p => p.theme.space[6] }px;
    }
  }
`;

export function Sidebar(props: SidebarProps) {
  const { invites, api, associations, selected, apps, workspace } = props;
  const groupPath = getGroupFromWorkspace(workspace);
  const display = props.mobileHide ? ["none", "flex"] : "flex";
  if (!associations) {
    return null;
  }

  const [config, setConfig] = useLocalStorageState<SidebarListConfig>(
    `group-config:${groupPath || "home"}`,
    {
      sortBy: "asc",
      hideUnjoined: false,
    }
  );
  return (
    <Col
      display={display}
      width="100%"
      gridRow="1/2"
      gridColumn="1/2"
      borderRight={1}
      borderRightColor="washedGray"
      overflowY="scroll"
      fontSize={0}
      bg="white"
      position="relative"
    >
      <GroupSwitcher
        associations={associations}
        recentGroups={props.recentGroups}
        baseUrl={props.baseUrl}
        workspace={props.workspace}
      />
      <SidebarListHeader initialValues={config} handleSubmit={setConfig} />
      <SidebarList
        config={config}
        associations={associations}
        selected={selected}
        group={groupPath}
        groups={props.groups}
        apps={props.apps}
        baseUrl={props.baseUrl}
      />
      <SidebarStickySpacer flexShrink={0} />
      <Box
        flexShrink="0"
        display="flex"
        justifyContent="center"
        position="sticky"
        bottom={"8px"}
        width="100%"
        height="fit-content"
        py="2"
      >
        <Link
          to={!!groupPath ? `/~landscape${groupPath}/new` : `/~landscape/home/new`}
        >
          <Box
            bg="white"
            p={2}
            borderRadius={1}
            border={1}
            borderColor="lightGray"
          >
            + New Channel
          </Box>
        </Link>
      </Box>
    </Col>
  );
}
