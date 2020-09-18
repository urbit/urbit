import React from "react";

import { Icon, Row, Box, Text } from "@tlon/indigo-react";

import { Association } from "~/types/metadata-update";

import { SidebarAppConfigs, SidebarItemStatus } from "./Sidebar";
import { HoverBoxLink } from "./HoverBox";

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

const getAppIcon = (app: string) => {
  if (app === "link") {
    return "Links";
  }
  return _.capitalize(app);
};

export function SidebarItem(props: {
  hideUnjoined: boolean;
  association: Association;
  path: string;
  selected: boolean;
  apps: SidebarAppConfigs;
}) {
  const { association, path, selected, apps } = props;
  const title = association?.metadata?.title || path;
  const appName = association?.["app-name"];
  const appPath = association?.["app-path"];
  const groupPath = association?.["group-path"];
  const app = apps[appName];
  const status = app.getStatus(path);
  const hasUnread = status === "unread" || status === "mention";

  const isSynced = status !== "unsubscribed";

  const to = isSynced
    ? `/~groups${groupPath}/resource/${appName}${appPath}`
    : `/~groups${groupPath}/join/${appName}${appPath}`;

  const color = selected ? 'black' : isSynced ? 'gray' : 'lightGray';

  if(props.hideUnjoined && !isSynced) {
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
        <Icon display="block" fill="rgba(0,0,0,0)" stroke={color} icon={getAppIcon(appName)} />
        <Box flexShrink={2} ml={2} lineHeight="1.33" fontWeight={hasUnread ? "600" : "400"}>
          <Text color={selected || isSynced ? 'black' : 'lightGray'}>
            {title}
          </Text>
        </Box>
      </Row>
    </HoverBoxLink>
  );
}
