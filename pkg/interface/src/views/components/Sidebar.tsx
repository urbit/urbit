import React, { ReactNode, useState } from "react";
import {
  Box,
  Row,
  Text,
  Icon,
  MenuItem as _MenuItem,
  IconButton,
  Button,
} from "@tlon/indigo-react";
import { capitalize } from "lodash";
import { Link } from "react-router-dom";

import { SidebarInvite } from "./SidebarInvite";
import GlobalApi from "~/logic/api/global";
import { AppName } from "~/types/noun";
import { alphabeticalOrder } from "~/logic/lib/util";
import { GroupSwitcher } from "~/views/apps/groups/components/GroupSwitcher";
import { AppInvites, Associations, AppAssociations, Workspace } from "~/types";
import { SidebarItem } from "./SidebarItem";
import {
  SidebarListHeader,
  SidebarListConfig,
  SidebarSort,
} from "./SidebarListHeader";
import { useLocalStorageState } from "~/logic/lib/useLocalStorageState";
import {getGroupFromWorkspace} from "~/logic/lib/workspace";

interface SidebarAppConfig {
  name: string;
  makeRouteForResource: (appPath: string) => string;
  getStatus: (appPath: string) => SidebarItemStatus | undefined;
}

export type SidebarAppConfigs = { [a in AppName]: SidebarAppConfig };

export type SidebarItemStatus =
  | "unread"
  | "mention"
  | "unsubscribed"
  | "disconnected"
  | "loading";

function sidebarSort(
  associations: AppAssociations
): Record<SidebarSort, (a: string, b: string) => number> {
  const alphabetical = (a: string, b: string) => {
    const aAssoc = associations[a];
    const bAssoc = associations[b];
    const aTitle = aAssoc?.metadata?.title || b;
    const bTitle = bAssoc?.metadata?.title || b;

    return alphabeticalOrder(aTitle, bTitle);
  };

  return {
    asc: alphabetical,
    desc: (a, b) => alphabetical(b, a),
  };
}

const apps = ["chat", "publish", "link"];

function SidebarItems(props: {
  apps: SidebarAppConfigs;
  config: SidebarListConfig;
  associations: Associations;
  group?: string;
  selected?: string;
}) {
  const { selected, group, config } = props;
  const associations = {
    ...props.associations.chat,
    ...props.associations.publish,
    ...props.associations.link,
    ...props.associations.graph,
  };

  const ordered = Object.keys(associations)
    .filter((a) => {
      const assoc = associations[a];
      console.log(a);
      return group
        ? assoc["group-path"] === group
        : !(assoc["group-path"] in props.associations.contacts);
    })
    .sort(sidebarSort(associations)[config.sortBy]);

  return (
    <>
      {ordered.map((path) => {
        const assoc = associations[path];
        return (
          <SidebarItem
            key={path}
            path={path}
            selected={path === selected}
            association={assoc}
            apps={props.apps}
            hideUnjoined={config.hideUnjoined}
          />
        );
      })}
    </>
  );
}

interface SidebarProps {
  children: ReactNode;
  recentGroups: string[];
  invites: AppInvites;
  api: GlobalApi;
  associations: Associations;
  selected?: string;
  selectedGroup?: string;
  includeUnmanaged?: boolean;
  apps: SidebarAppConfigs;
  baseUrl: string;
  mobileHide?: boolean;
  workspace: Workspace;
}

export function Sidebar(props: SidebarProps) {
  const { invites, api, associations, selected, apps, workspace } = props;
  const groupPath = getGroupFromWorkspace(workspace)
  const groupAsssociation =
    groupPath && associations.contacts[groupPath];
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
    <Box
      display={display}
      flexDirection="column"
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
      <SidebarItems
        config={config}
        associations={associations}
        selected={selected}
        group={groupPath}
        apps={props.apps}
      />
      <Box
        display="flex"
        justifyContent="center"
        position="sticky"
        bottom="8px"
        width="100%"
        my={2}
      >
        <Link to={`/~groups${props.selectedGroup}/new`}>
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
    </Box>
  );
}
