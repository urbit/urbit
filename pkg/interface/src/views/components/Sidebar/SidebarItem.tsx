import React from "react";
import _ from 'lodash';

import { Icon, Row, Box, Text } from "@tlon/indigo-react";

import { Association } from "~/types/metadata-update";

import { SidebarAppConfigs, SidebarItemStatus } from "./Sidebar";
import { HoverBoxLink } from "../HoverBox";
import { Groups } from "~/types";

function SidebarItemIndicator(props: { status?: SidebarItemStatus }) {
  switch (props.status) {
    case "disconnected":
      return <Icon ml={2} fill="red" icon="X" />;
    case "unsubscribed":
      return <Icon ml={2} icon="Circle" fill="gray" />;
    case "mention":
      return <Icon ml={2} icon="Circle" />;
    case "loading":
      return <Icon ml={2} icon="Bullet" />;
    default:
      return null;
  }
}

const getAppIcon = (app: string, module: string) => {
  if (app === "graph") {
    if (module === "link") {
      return "Links";
    }
    return _.capitalize(module);
  }
  return _.capitalize(app);
};

export function SidebarItem(props: {
  hideUnjoined: boolean;
  association: Association;
  groups: Groups;
  path: string;
  selected: boolean;
  apps: SidebarAppConfigs;
}) {
  const { association, path, selected, apps, groups } = props;
  const title = association?.metadata?.title || path;
  const appName = association?.["app-name"];
  const module = association?.metadata?.module || appName;
  const appPath = association?.["app-path"];
  const groupPath = association?.["group-path"];
  const app = apps[module];
  const isUnmanaged = groups?.[groupPath]?.hidden || false;
  if (!app) {
    return null;
  }
  const itemStatus = app.getStatus(path);
  const hasUnread = itemStatus === "unread" || itemStatus === "mention";

  const isSynced = itemStatus !== "unsubscribed";

  const baseUrl = isUnmanaged ? `/~groups/home` : `/~groups${groupPath}`;

  const to = isSynced
    ? `${baseUrl}/resource/${module}${appPath}`
    : `${baseUrl}/join/${module}${appPath}`;

  const color = selected ? "black" : isSynced ? "gray" : "lightGray";

  if (props.hideUnjoined && !isSynced) {
    return null;
  }

  return (
    <HoverBoxLink
      to={to}
      bg="white"
      bgActive="washedGray"
      width="100%"
      display="flex"
      justifyContent="space-between"
      alignItems="center"
      py={1}
      pl={4}
      pr={2}
      selected={selected}
    >
      <Row alignItems="center">
        <Icon
          display="block"
          color="transparent"
          stroke={color}
          icon={getAppIcon(appName, module) as any}
        />
        <Box flexShrink={2} ml={2}>
          <Text
            lineHeight="1.33"
            fontWeight={hasUnread ? "600" : "400"}
            color={selected || isSynced ? "black" : "lightGray"}
          >
            {title}
          </Text>
        </Box>
      </Row>
    </HoverBoxLink>
  );
}
