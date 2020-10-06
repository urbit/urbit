import React, { ReactNode, useState } from "react";
import styled from 'styled-components';
import {
  Box,
  Row,
  Text,
  Icon,
  MenuItem as _MenuItem,
  IconButton,
  Button,
  Col,
} from "@tlon/indigo-react";
import { capitalize } from "lodash";
import { Link } from "react-router-dom";

import { SidebarInvite } from "./SidebarInvite";
import GlobalApi from "~/logic/api/global";
import { AppName } from "~/types/noun";
import { alphabeticalOrder } from "~/logic/lib/util";
import { GroupSwitcher } from "../GroupSwitcher";
import {
  AppInvites,
  Associations,
  AppAssociations,
  Workspace,
  Groups,
} from "~/types";
import { SidebarItem } from "./SidebarItem";
import {
  SidebarListHeader,
  SidebarListConfig,
  SidebarSort,
} from "./SidebarListHeader";
import { useLocalStorageState } from "~/logic/lib/useLocalStorageState";
import { getGroupFromWorkspace } from "~/logic/lib/workspace";
import { SidebarAppConfigs } from './types';
import {SidebarList} from "./SidebarList";

const apps = ["chat", "publish", "link"];

interface SidebarProps {
  children: ReactNode;
  recentGroups: string[];
  invites: AppInvites;
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
      height="100%"
      gridRow="1/2"
      gridColumn="1/2"
      borderRight={1}
      borderRightColor="washedGray"
      overflowY="auto"
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
      {Object.keys(invites).map((appPath) =>
        Object.keys(invites[appPath]).map((uid) => (
          <SidebarInvite
            key={uid}
            invite={props.invites[uid]}
            onAccept={() => props.api.invite.accept(appPath, uid)}
            onDecline={() => props.api.invite.decline(appPath, uid)}
          />
        ))
      )}
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
        display="flex"
        justifyContent="center"
        position="sticky"
        bottom="8px"
        width="100%"
        my={2}
      >
        <Link
          to={!!groupPath ? `/~groups${groupPath}/new` : `/~groups/home/new`}
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
