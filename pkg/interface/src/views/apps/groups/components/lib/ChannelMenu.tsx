import React, { useCallback } from "react";
import { Link } from "react-router-dom";

import { Icon, Row, Col, Button, Text, Box } from "@tlon/indigo-react";
import { Dropdown } from "~/views/components/Dropdown";
import { Association } from "~/types";
import GlobalApi from "~/logic/api/global";

const ChannelMenuItem = ({
  icon,
  stroke = undefined,
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
    <Icon fill={stroke} icon={icon} />
    {children}
  </Row>
);

interface ChannelMenuProps {
  association: Association;
  api: GlobalApi;
}

export function ChannelMenu(props: ChannelMenuProps) {
  const { association, api } = props;
  const { metadata } = association;
  const app = metadata.module || association["app-name"];
  const baseUrl = `/~groups${association?.["group-path"]}/resource/${app}${association["app-path"]}`;
  const appPath = association["app-path"];

  const [, ship, name] = appPath.startsWith('/ship/') ? appPath.slice(5).split("/") : appPath.split('/');

  const isOurs = ship.slice(1) === window.ship;
  const onUnsubscribe = useCallback(async () => {
    const app = metadata.module || association["app-name"];
    switch (app) {
      case "chat":
        await api.chat.delete(appPath);
        break;
      case "publish":
        await api.publish.unsubscribeNotebook(ship.slice(1), name);
        break;
      case "link":
        await api.graph.leaveGraph(ship.slice(1), name);
        break;
      default:
        throw new Error("Invalid app name");
    }
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
  }, [api, association]);

  return (
    <Dropdown
      options={
        <Col>
          <ChannelMenuItem icon="Gear">
            <Link to={`${baseUrl}/settings`}>
              <Box fontSize={0} p="2">
                Channel Settings
              </Box>
            </Link>
          </ChannelMenuItem>
          {isOurs ? (
            <ChannelMenuItem stroke="red" bottom icon="TrashCan">
              <Button error onClick={onDelete}>
                Delete Channel
              </Button>
            </ChannelMenuItem>
          ) : (
            <ChannelMenuItem bottom icon="ArrowEast">
              <Button error={isOurs} onClick={onUnsubscribe}>
                Unsubscribe from Channel
              </Button>
            </ChannelMenuItem>
          )}
        </Col>
      }
      alignX="right"
      alignY="top"
      width="250px"
    >
      <Icon icon="Menu" stroke="gray" />
    </Dropdown>
  );
}
