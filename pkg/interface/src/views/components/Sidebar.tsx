import React, { ReactNode, useState } from "react";
import {
  Box,
  Row,
  Text,
  Icon,
  MenuItem as _MenuItem,
} from "@tlon/indigo-react";
import { capitalize } from "lodash";

import { SidebarInvite } from "./SidebarInvite";
import GlobalApi from "~/logic/api/global";
import { AppName } from "~/types/noun";
import { alphabeticalOrder } from "~/logic/lib/util";
import { GroupSwitcher } from "~/views/apps/groups/components/GroupSwitcher";
import { AppInvites, Associations, AppAssociations } from "~/types";
import { SidebarItem } from "./SidebarItem";

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

function ItemGroup(props: {
  app: string;
  apps: SidebarAppConfigs;
  associations: Associations;
  selected?: string;
  group: string;
}) {
  const { selected, apps, associations } = props;
  const [open, setOpen] = useState(true);

  const toggleOpen = () => setOpen((o) => !o);

  const assoc = associations[props.app as AppName];

  const items = _.pickBy(
    assoc,
    (value) =>
      value["group-path"] === props.group && value["app-name"] === props.app
  );

  if (Object.keys(items).length === 0) {
    return null;
  }

  return (
    <Box mb={3}>
      <Row alignItems="center" onClick={toggleOpen} pl={2} mb={1}>
        <Icon
          mb="1px"
          fill="lightGray"
          icon={open ? "TriangleSouth" : "TriangleEast"}
        />
        <Text pl={1} color="lightGray">
          {capitalize(props.app)}
        </Text>
      </Row>
      {open && <SidebarItems selected={selected} items={items} apps={apps} />}
    </Box>
  );
}

const apps = ["chat", "publish", "link"];
const GroupItems = (props: {
  associations: Associations;
  group: string;
  apps: SidebarAppConfigs;
  selected?: string;
}) => (
  <>
    {apps.map((app) => (
      <ItemGroup app={app} {...props} />
    ))}
  </>
);

function SidebarItems(props: {
  apps: SidebarAppConfigs;
  items: AppAssociations;
  selected?: string;
}) {
  const { items, associations, selected } = props;

  const ordered = Object.keys(items).sort((a, b) => {
    const aAssoc = items[a];
    const bAssoc = items[b];
    const aTitle = aAssoc?.metadata?.title || b;
    const bTitle = bAssoc?.metadata?.title || b;

    return alphabeticalOrder(aTitle, bTitle);
  });

  return (
    <>
      {ordered.map((path) => {
        const assoc = items[path];
        return (
          <SidebarItem
            key={path}
            path={path}
            selected={path === selected}
            association={assoc}
            apps={props.apps}
          />
        );
      })}
    </>
  );
}

interface SidebarProps {
  children: ReactNode;
  invites: AppInvites;
  api: GlobalApi;
  associations: Associations;
  selected?: string;
  selectedGroup: string;
  apps: SidebarAppConfigs;
  baseUrl: string;
  mobileHide?: boolean;
}

export function Sidebar(props: SidebarProps) {
  const { invites, api, associations, selected, apps } = props;
  const groupAsssociation = associations.contacts[props.selectedGroup];
  const display = props.mobileHide ? ["none", "flex"] : "flex";
  if (!groupAsssociation) {
    return null;
  }
  return (
    <Box
      display={display}
      flexDirection="column"
      width="100%"
      gridRow="1/3"
      gridColumn="1/2"
      borderRight={1}
      borderRightColor="washedGray"
      overflowY="auto"
      fontSize={0}
      bg="white"
      position="relative"
    >
      <GroupSwitcher baseUrl={props.baseUrl} association={groupAsssociation} />
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
      <GroupItems
        group={props.selectedGroup}
        apps={apps}
        selected={selected}
        associations={associations || {}}
      />
    </Box>
  );
}
