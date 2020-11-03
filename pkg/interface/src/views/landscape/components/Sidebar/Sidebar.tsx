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
  Rolodex,
} from "~/types";
import { SidebarListHeader } from "./SidebarListHeader";
import { useLocalStorageState } from "~/logic/lib/useLocalStorageState";
import { getGroupFromWorkspace } from "~/logic/lib/workspace";
import { SidebarAppConfigs } from './types';
import { SidebarList } from "./SidebarList";
import { SidebarInvite } from './SidebarInvite';
import { roleForShip } from "~/logic/lib/group";


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

const inviteItems = (invites, api) => {
  const returned = [];
  Object.keys(invites).filter((e) => {
    return e !== '/contacts';
  }).map((appKey) => {
    const app = invites[appKey];
    Object.keys(app).map((uid) => {
      const invite = app[uid];
      const inviteItem =
        <SidebarInvite
          key={uid}
          invite={invite}
          onAccept={() => api.invite.accept(appKey, uid)}
          onDecline={() => api.invite.decline(appKey, uid)}
        />;
        returned.push(inviteItem);
    });
  });
  return returned;
};

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
  const sidebarInvites = (workspace?.type === 'home')
    ? inviteItems(invites, api) : null;

  const role = props.groups?.[groupPath] ? roleForShip(props.groups[groupPath], window.ship) : undefined;
  const isAdmin = (role === "admin") || (workspace?.type === 'home');

  const newStyle = {
    display: isAdmin ? "block" : "none"
  };

  return (
    <Col
      display={display}
      width="100%"
      gridRow="1/2"
      gridColumn="1/2"
      borderTopLeftRadius='2'
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
      <SidebarListHeader
        contacts={props.contacts} 
        baseUrl={props.baseUrl}
        groups={props.groups}
        initialValues={config}
        handleSubmit={setConfig}
        selected={selected || ""} 
        workspace={workspace} />
      {sidebarInvites}
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
          style={newStyle}
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
