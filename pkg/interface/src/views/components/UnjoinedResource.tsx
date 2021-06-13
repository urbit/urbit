import { Box, Col, Text, Row, Icon, Center } from '@tlon/indigo-react';
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
import Author from '~/views/components/Author';

interface UnjoinedResourceProps {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
  modalContext?: boolean;
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
  const { api, modalContext } = props;
  const history = useHistory();
  const { query } = useQuery();
  const rid = props.association.resource;
  const appName = props.association['app-name'];

  const { title, description, config, creator } = props.association.metadata;
  const graphKeys = useGraphState(state => state.graphKeys);

  const [loading, setLoading] = useState(false);
  const waiter = useWaitForProps({ ...props, graphKeys });
  const app = useMemo(() => (config as GraphConfig).graph || appName, [
    props.association
  ]);

  const onJoin = async () => {
    const [, , ship, name] = rid.split('/');
    await api.graph.joinGraph(ship, name);
    await waiter(isJoined(rid));
    const redir = query.get('redir') ?? `${props.baseUrl}/resource/${app}${rid}`;
    history.push(redir);
  };

  const resourceIcon = (app) => {
    switch (app) {
      case 'chat':
        return <Icon icon="Chat" />;
      case 'link':
        return <Icon icon="Collection" />;
      case 'publish':
        return <Icon icon="Publish" />;
      default:
        return <Icon icon="Groups" />;
    }
  };

  useEffect(() => {
    if (isJoined(rid)({ graphKeys })) {
      history.push(`${props.baseUrl}/resource/${app}${rid}`);
    }
  }, [props.association, graphKeys]);

  useEffect(() => {
    (async () => {
      if (query.has('auto')) {
        setLoading(true);
        await onJoin();
        setLoading(false);
      }
    })();
  }, [query]);

  return (
    <Center p={modalContext ? '0' : 6}>
      <Col width="100%" maxWidth={!modalContext ? '500px' : null} p={4}>
        <Box mb={4}>
          <Text fontSize={3} color="black" fontWeight="600">
            Join{loading ? 'ing' : null} Channel
          </Text>
        </Box>
        <Box p={3} backgroundColor="washedGray" borderRadius={2} mb={4}>
          <Row
            alignItems={description ? 'top' : 'center'}
            pb={3}
            borderBottom="1px solid"
            borderColor="lightGray"
            mb={3}
          >
            <Col mr={2}>
              <Box background="lightGray" size='36px' borderRadius={1}>
                <Center height='36px'>{resourceIcon(app)}</Center>
              </Box>
            </Col>
            <Col>
              <Text fontWeight="500">
                {title}
              </Text>
              <RichText color="gray" api={api} mb="0">
                {description}
              </RichText>
            </Col>
          </Row>
          <Col>
            <Text mb={3} color="gray">Channel Host</Text>
            <Author
              ship={creator}
              showImage={true}
              size={24}
              sigilPadding={6}
            />
          </Col>
          {console.log(props.association)}
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
