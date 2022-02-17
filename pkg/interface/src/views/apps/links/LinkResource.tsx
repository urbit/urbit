import { Box, Center, Col, Icon, LoadingSpinner, Text } from '@tlon/indigo-react';
import { Association, deSig, Group } from '@urbit/api';
import bigInt from 'big-integer';
import React, { useEffect, useMemo } from 'react';
import { Link, Route, Switch, useLocation } from 'react-router-dom';
import { useQuery } from '~/logic/lib/useQuery';
import { Titlebar } from '~/views/components/Titlebar';
import useGraphState from '~/logic/state/graph';
import useMetadataState from '~/logic/state/metadata';
import useGroupState from '../../../logic/state/group';
import { LinkBlocks } from './components/LinkBlocks';
import { LinkDetail } from './components/LinkDetail';
import './css/custom.css';
import LinkWindow from './LinkWindow';
import { getPermalinkForGraph } from '~/logic/lib/permalinks';
import { useCopy } from '~/logic/lib/useCopy';

interface LinkResourceProps {
  association: Association;
  baseUrl: string;
}

export function LinkResource(props: LinkResourceProps) {
  const {
    association,
    baseUrl
  } = props;

  const rid = association.resource;

  const relativePath = (p: string) => `${baseUrl}/resource/link${rid}${p}`;
  const associations = useMetadataState(state => state.associations);

  const [, , ship, name] = rid.split('/');
  const resourcePath = `${deSig(ship)}/${name}`;
  const permalink = useMemo(() => getPermalinkForGraph(association.group, association.resource), [association]);
  const { doCopy, copyDisplay } = useCopy(permalink, <Icon icon='Copy' color='gray' pr={2} />, <Icon icon='Checkmark' color='gray' pr={2} />);
  const resource: any = associations.graph[rid]
    ? associations.graph[rid]
    : { metadata: {} };
  const groups = useGroupState(state => state.groups);
  const group = groups[resource?.group] || {};

  const graphs = useGraphState(state => state.graphs);
  const graph = graphs[resourcePath] || null;
  const graphTimesentMap = useGraphState(state => state.graphTimesentMap);
  const { query } = useQuery();
  const isGrid = query.has('grid');
  const { pathname, search } = useLocation();
  const getGraph = useGraphState(s => s.getGraph);

  useEffect(() => {
    getGraph(ship, name);
  }, [association]);

  const resourceUrl = `${baseUrl}/resource/link${rid}`;
  if (!graph || !resource) {
    return <Center width='100%' height='100%'><LoadingSpinner /></Center>;
  }
  const { title, description } = resource.metadata;

  const copyControl = (
    <Box onClick={doCopy} cursor="pointer">
      {copyDisplay}
    </Box>
  );

  const titlebar = (back?: string) => (
    <Titlebar back={back && `${back}${search}`} title={title} description={description} workspace={baseUrl} baseUrl={resourceUrl} >
      <Link to={{ pathname, search: isGrid ? '' : '?grid=true' }}>
        <Text bold pr='3' color='blue'>
          Switch to {isGrid ? 'list' : 'grid' }
        </Text>
      </Link>
      {copyControl}
    </Titlebar>
  );

  return (
    <Switch>
      <Route
        exact
        path={relativePath('')}
        render={(props) => {
          return (
            <Col minWidth="0" overflow="hidden">
              {titlebar()}
              { !isGrid ?  /* @ts-ignore withState typings */ (
                  <LinkWindow
                    key={rid}
                    association={resource}
                    resource={resourcePath}
                    graph={graph}
                    baseUrl={resourceUrl}
                    group={group as Group}
                    path={resource.group}
                    pendingSize={Object.keys(graphTimesentMap[resourcePath] || {}).length}
                    mb={3}
                  />
              ) : (
                <LinkBlocks key={rid} graph={graph} association={resource} />
              )}
          </Col>
          );
        }}
      />
      <Route
        path={relativePath('/index/:index')}
        render={(props) => {
          const index = bigInt(props.match.params.index);

          if (!index) {
            return <div>Malformed URL</div>;
          }

          const node = graph ? graph.get(index) : null;

          if (!node) {
            return <Box>Not found</Box>;
          }

          if (typeof node.post === 'string') {
            return (
              <Col width="100%" textAlign="center" pt="2">
                <Text gray>This link has been deleted.</Text>
              </Col>
            );
          }
          return (
            <Col overflow="hidden">
              {titlebar(relativePath(''))}
              <LinkDetail
                node={node}
                association={association}
                baseUrl={pathname}
                flexGrow={1}
                maxHeight="calc(100% - 48px)"
              />
            </Col>
          );
        }}
      />
    </Switch>
  );
}
