import React, { useEffect, useMemo } from 'react';
import { useHistory } from 'react-router-dom';

import { Notebooks, Inbox, Association } from '@urbit/api';
import { Box, Text, Col, Center } from '@tlon/indigo-react';

import RichText from '~/views/components/RichText';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { StatelessAsyncButton } from './StatelessAsyncButton';
import useApi from '~/logic/lib/useApi';
import { joinGraph } from '@urbit/api';

interface UnjoinedResourceProps {
  association: Association;
  baseUrl: string;
  notebooks: Notebooks;
  graphKeys: Set<string>;
  inbox: Inbox;
}

function isJoined(path: string) {
  return function (
    props: Pick<UnjoinedResourceProps, 'graphKeys'>
  ) {
    const graphKey = path.substr(7);
    return props.graphKeys.has(graphKey);
  };
}

export function UnjoinedResource(props: UnjoinedResourceProps) {
  const { notebooks, graphKeys, inbox } = props;
  const history = useHistory();
  const rid = props.association.resource;
  const appName = props.association['app-name'];
  const { title, description, module } = props.association.metadata;
  const waiter = useWaitForProps(props);
  const app = useMemo(() => module || appName, [props.association]);
  const api = useApi();

  const onJoin = async () => {
    const [, , ship, name] = rid.split('/');
    await api.thread(joinGraph(ship, name));
    await waiter(isJoined(rid));
    history.push(`${props.baseUrl}/resource/${app}${rid}`);
  };

  useEffect(() => {
    if (isJoined(rid)({ graphKeys })) {
      history.push(`${props.baseUrl}/resource/${app}${rid}`);
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
          name={rid}
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
