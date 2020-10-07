import React, { useEffect, useMemo } from "react";
import { Association } from "~/types/metadata-update";
import { Box, Text, Button, Col, Center } from "@tlon/indigo-react";
import RichText from "~/views/components/RichText";
import { Link, useHistory } from "react-router-dom";
import GlobalApi from "~/logic/api/global";
import { useWaitForProps } from "~/logic/lib/useWaitForProps";
import {
  StatelessAsyncButton as AsyncButton,
  StatelessAsyncButton,
} from "./StatelessAsyncButton";
import { Notebooks, Graphs, Inbox } from "~/types";

interface UnjoinedResourceProps {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
  notebooks: Notebooks;
  graphKeys: Set<string>;
  inbox: Inbox;
}

function isJoined(app: string, path: string) {
  return function (
    props: Pick<UnjoinedResourceProps, "inbox" | "graphKeys" | "notebooks">
  ) {
    let ship, name;
    switch (app) {
      case "link":
        [, , ship, name] = path.split("/");
        return props.graphKeys.has(path);
      case "publish":
        [, ship, name] = path.split("/");
        return !!props.notebooks[ship]?.[name];
      case "chat":
        return !!props.inbox[path];
      default:
        console.log("Bad app name");
        return false;
    }
  };
}

export function UnjoinedResource(props: UnjoinedResourceProps) {
  const { api, notebooks, graphKeys, inbox } = props;
  const history = useHistory();
  const appPath = props.association["app-path"];
  const appName = props.association["app-name"];
  const { title, description, module } = props.association.metadata;
  const waiter = useWaitForProps(props);
  const app = useMemo(() => module || appName, [props.association]);

  const onJoin = async () => {
    let ship, name;
    switch (app) {
      case "link":
        [, , ship, name] = appPath.split("/");
        await api.graph.joinGraph(ship, name);
        break;
      case "publish":
        [, ship, name] = appPath.split("/");
        await api.publish.subscribeNotebook(ship.slice(1), name);
        break;
      case "chat":
        [, ship, name] = appPath.split("/");
        await api.chat.join(ship, appPath, true);
        break;
      default:
        throw new Error("Unknown resource type");
    }
    await waiter(isJoined(app, appPath));
    history.push(`${props.baseUrl}/resource/${app}${appPath}`);
  };

  useEffect(() => {
    if (isJoined(app, appPath)({ inbox, graphKeys, notebooks })) {
      history.push(`${props.baseUrl}/resource/${app}${appPath}`);
    }
  }, [props.association, inbox, graphKeys, notebooks]);

  return (
    <Center p={6}>
      <Col
        maxWidth="400px"
        p={4}
        border={1}
        borderColor="lightGray"
        borderRadius="1"
        gapY="3"
      >
        <Box>
          <Text>{title}</Text>
        </Box>
        <Box>
          <RichText color="gray">{description}</RichText>
        </Box>
        <StatelessAsyncButton
          name={appPath}
          primary
          width="fit-content"
          onClick={onJoin}
        >
          Join Channel
        </StatelessAsyncButton>
      </Col>
    </Center>
  );
}
