import React from "react";
import { Association } from "~/types/metadata-update";
import { Box, Text, Button, Col, Center } from "@tlon/indigo-react";
import { Link, useHistory } from "react-router-dom";
import GlobalApi from "~/logic/api/global";
import {useWaitForProps} from "~/logic/lib/useWaitForProps";
import { StatelessAsyncButton as AsyncButton } from './StatelessAsyncButton';
import {Notebooks, Graphs, Inbox} from "~/types";

interface UnjoinedResourceProps {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
  notebooks: Notebooks;
  graphs: Graphs;
  inbox: Inbox;
}

export function UnjoinedResource(props: UnjoinedResourceProps) {
  const { api } = props;
  const history = useHistory();
  const appPath = props.association["app-path"];
  const appName = props.association["app-name"];
  const { title, description, module } = props.association.metadata;
  const waiter = useWaitForProps(props);
  const app = module || appName;

  const onJoin = async () => {
    let ship, name;
    switch(app) {
      case 'link':
        [,,ship,name] = appPath.split('/');
        await api.graph.joinGraph(ship, name);
        break;
      case 'publish': 
        [,ship,name] = appPath.split('/');
        await api.publish.subscribeNotebook(ship.slice(1), name);
        await waiter(p => !!p?.notebooks?.[ship]?.[name])
        break;
      case 'chat':
        [,ship,name] = appPath.split('/');
        await api.chat.join(ship, appPath, true);
        await waiter(p => !!p?.inbox?.[appPath])
        break;
      default:
        throw new Error("Unknown resource type");
    }
    history.push(`${props.baseUrl}/resource/${app}${appPath}`);
  };
  return (
    <Center p={6}>
      <Col maxWidth="400px" p={4} border={1} borderColor="washedGray">
        <Box mb={4}>
          <Text>{title}</Text>
        </Box>
        <Box mb={4}>
          <Text color="gray">{description}</Text>
        </Box>
        <AsyncButton onClick={onJoin} mx="auto" border>
            Join Channel
        </AsyncButton>
      </Col>
    </Center>
  );
}
