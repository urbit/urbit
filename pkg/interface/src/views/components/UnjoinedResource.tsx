import { Box, Center, Col, Text } from '@tlon/indigo-react';
import { Association, GraphConfig } from '@urbit/api/metadata';
import React, { useEffect, useMemo, useState } from 'react';
import { useHistory } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import { useQuery } from '~/logic/lib/useQuery';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import useGraphState from '~/logic/state/graph';
import RichText from '~/views/components/RichText';
import {
  StatelessAsyncButton
} from './StatelessAsyncButton';

interface UnjoinedResourceProps {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
}

function isJoined(path: string) {
  return function (
    props: { graphKeys: Set<string> }
  ) {
    const graphKey = path.substr(7);
    return props.graphKeys.has(graphKey);
  };
}

export function UnjoinedResource(props: UnjoinedResourceProps) {
  const { api } = props;
  const history = useHistory();
  const { query } = useQuery();
  const rid = props.association.resource;
  const appName = props.association['app-name'];

  const { title, description, config } = props.association.metadata;
  const graphKeys = useGraphState(state => state.graphKeys);

  const [loading, setLoading] = useState(false);
  const waiter = useWaitForProps({ ...props, graphKeys });
  const app = useMemo(() => (config as GraphConfig).graph || appName, [props.association]);

  const onJoin = async () => {
    const [, , ship, name] = rid.split('/');
    await api.graph.joinGraph(ship, name);
    await waiter(isJoined(rid));
    const redir = query.get('redir') ?? `${props.baseUrl}/resource/${app}${rid}`;
    history.push(redir);
  };

  useEffect(() => {
    if (isJoined(rid)({ graphKeys })) {
      history.push(`${props.baseUrl}/resource/${app}${rid}`);
    }
  }, [props.association, graphKeys]);

  useEffect(() => {
    (async () => {
      if(query.has('auto')) {
        setLoading(true);
        await onJoin();
        setLoading(false);
      }
    })();
  }, [query]);

  return (
    <Center p={6} className='unjoined-resource'>
      <Col
        maxWidth="400px"
        p={4}
        border={1}
        borderColor="lightGray"
        borderRadius={1}
        gapY={3}
      >
        <Box>
          <Text>{title}</Text>
        </Box>
        <Box>
          <RichText color="gray" api={api}>{description}</RichText>
        </Box>
        <StatelessAsyncButton
          name={rid}
          primary
          loading={loading}
          width="fit-content"
          onClick={onJoin}
        >
          Join Channel
        </StatelessAsyncButton>
      </Col>
    </Center>
  );
}
