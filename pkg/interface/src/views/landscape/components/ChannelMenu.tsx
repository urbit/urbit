import React, { useCallback } from "react";
import { Link, useHistory } from "react-router-dom";

import { Icon, Row, Col, Button, Text, Box, Action } from "@tlon/indigo-react";
import { Dropdown } from "~/views/components/Dropdown";
import { Association } from "~/types";
import GlobalApi from "~/logic/api/global";

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
}

export function ChannelMenu(props: ChannelMenuProps) {
  const { association, api } = props;
  const history = useHistory();
  const { metadata } = association;
  const app = metadata.module || association["app-name"];
  const baseUrl = `/~landscape${association?.["group-path"]}/resource/${app}${association["app-path"]}`;
  const appPath = association["app-path"];

  const [, ship, name] = appPath.startsWith("/ship/")
    ? appPath.slice(5).split("/")
    : appPath.split("/");

  const isOurs = ship.slice(1) === window.ship;
  const onUnsubscribe = useCallback(async () => {
    const app = metadata.module || association["app-name"];
    switch (app) {
      case "chat":
        await api.chat.delete(appPath);
        break;
      case "publish":
        await api.publish.unsubscribeNotebook(ship.slice(1), name);
        await api.publish.fetchNotebooks();

        break;
      case "link":
        await api.graph.leaveGraph(ship, name);
        break;
      default:
        throw new Error("Invalid app name");
    }
    history.push(`/~landscape${association?.["group-path"]}`);
  }, [api, association]);

  const onDelete = useCallback(async () => {
    const app = metadata.module || association["app-name"];
    switch (app) {
      case "chat":
        await api.chat.delete(appPath);
        break;
      case "publish":
        await api.publish.delBook(name);
        break;
      case "link":
        await api.graph.deleteGraph(name);
        break;
      default:
        throw new Error("Invalid app name");
    }
    history.push(`/~landscape${association?.["group-path"]}`);
  }, [api, association]);

  return (
    <Dropdown
      options={
        <Col bg="white" border={1} borderRadius={1} borderColor="lightGray">
          {isOurs ? (
            <>
              <ChannelMenuItem color="red" icon="TrashCan">
                <Action m="2" destructive onClick={onDelete}>
                  Delete Channel
                </Action>
              </ChannelMenuItem>
              <ChannelMenuItem bottom icon="Gear">
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
