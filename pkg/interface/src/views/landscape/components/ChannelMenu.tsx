import React, { useCallback } from "react";
import { Link, useHistory } from "react-router-dom";

import { Icon, Row, Col, Button, Text, Box, Action } from "@tlon/indigo-react";
import { Dropdown } from "~/views/components/Dropdown";
import { Association, NotificationGraphConfig } from "~/types";
import GlobalApi from "~/logic/api/global";
import { StatelessAsyncAction } from "~/views/components/StatelessAsyncAction";
import { appIsGraph } from "~/logic/lib/util";

const ChannelMenuItem = ({
  icon,
  color = undefined as string | undefined,
  children,
  bottom = false,
}) => (
  <Row
    alignItems="center"
    borderBottom={bottom ? 0 : 1}
    borderBottomColor="lightGray"
    px={2}
    py={1}
  >
    <Icon color={color} icon={icon} />
    {children}
  </Row>
);

interface ChannelMenuProps {
  association: Association;
  api: GlobalApi;
  graphNotificationConfig: NotificationGraphConfig;
  chatNotificationConfig: string[];
}

export function ChannelMenu(props: ChannelMenuProps) {
  const { association, api } = props;
  const history = useHistory();
  const { metadata } = association;
  const app = metadata.module || association["app-name"];
  const workspace = history.location.pathname.startsWith("/~landscape/home")
    ? "/home"
    : association?.["group-path"];
  const baseUrl = `/~landscape${workspace}/resource/${app}${association["app-path"]}`;
  const appPath = association["app-path"];

  const [, ship, name] = appPath.startsWith("/ship/")
    ? appPath.slice(5).split("/")
    : appPath.split("/");

  const isOurs = ship.slice(1) === window.ship;

  const isMuted = appIsGraph(app)
    ? props.graphNotificationConfig.watching.findIndex(
        (a) => a.graph === appPath && a.index === "/"
      ) === -1
    : props.chatNotificationConfig.findIndex((a) => a === appPath) === -1;

  const onChangeMute = async () => {
    if (association["app-name"] === "chat") {
      const func = isMuted ? "listenChat" : "ignoreChat";
      return api.hark[func](appPath);
    }
    const func = isMuted ? "listenGraph" : "ignoreGraph";
    await api.hark[func](appPath, "/");
  };
  const onUnsubscribe = useCallback(async () => {
    const app = metadata.module || association["app-name"];
    switch (app) {
      case "chat":
        await api.chat.delete(appPath);
        break;
      case "publish":
        await api.graph.leaveGraph(ship, name);
        break;
      case "link":
        await api.graph.leaveGraph(ship, name);
        break;
      default:
        throw new Error("Invalid app name");
    }
    history.push(`/~landscape${workspace}`);
  }, [api, association]);

  const onDelete = useCallback(async () => {
    const app = metadata.module || association["app-name"];
    switch (app) {
      case "chat":
        await api.chat.delete(appPath);
        break;
      case "publish":
        await api.graph.deleteGraph(name);
        break;
      case "link":
        await api.graph.deleteGraph(name);
        break;
      default:
        throw new Error("Invalid app name");
    }
    history.push(`/~landscape${workspace}`);
  }, [api, association]);

  return (
    <Dropdown
      options={
        <Col
          backgroundColor="white"
          border={1}
          borderRadius={1}
          borderColor="lightGray"
        >
          <ChannelMenuItem color="blue" icon="Inbox">
            <StatelessAsyncAction
              m="2"
              bg="white"
              name="notif"
              onClick={onChangeMute}
            >
              {isMuted ? "Unmute" : "Mute"} this channel
            </StatelessAsyncAction>
          </ChannelMenuItem>
          {isOurs ? (
            <>
              <ChannelMenuItem color="red" icon="TrashCan">
                <Action
                  m="2"
                  backgroundColor="white"
                  destructive
                  onClick={onDelete}
                >
                  Delete Channel
                </Action>
              </ChannelMenuItem>
              <ChannelMenuItem bottom icon="Gear" color="black">
                <Link to={`${baseUrl}/settings`}>
                  <Box fontSize={0} p="2">
                    Channel Settings
                  </Box>
                </Link>
              </ChannelMenuItem>
            </>
          ) : (
            <ChannelMenuItem color="red" bottom icon="ArrowEast">
              <Action bg="white" m="2" destructive onClick={onUnsubscribe}>
                Unsubscribe from Channel
              </Action>
            </ChannelMenuItem>
          )}
        </Col>
      }
      alignX="right"
      alignY="top"
      width="250px"
    >
      <Icon display="block" icon="Menu" color="gray" />
    </Dropdown>
  );
}
