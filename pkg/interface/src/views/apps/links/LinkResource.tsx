import { Box, Center, Col, LoadingSpinner, Text } from '@tlon/indigo-react';
import { Group } from '@urbit/api';
import { Association } from '@urbit/api/metadata';
import bigInt from 'big-integer';
import React, { useEffect } from 'react';
import { Link, Redirect, Route, Switch } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import useGraphState from '~/logic/state/graph';
import useMetadataState from '~/logic/state/metadata';
import { StoreState } from '~/logic/store/type';
import {Titlebar} from '~/views/components/Titlebar';
import useGroupState from '../../../logic/state/group';
import { LinkBlocks } from './components/LinkBlocks';
import { LinkDetail } from './components/LinkDetail';
import './css/custom.css';
import LinkWindow from './LinkWindow';

type LinkResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
};

export function LinkResource(props: LinkResourceProps) {
  const {
    association,
    api,
    baseUrl
  } = props;

  const rid = association.resource;

  const relativePath = (p: string) => `${baseUrl}/resource/link${rid}${p}`;
  const associations = useMetadataState(state => state.associations);

  const [, , ship, name] = rid.split('/');
  const resourcePath = `${ship.slice(1)}/${name}`;
  const resource: any = associations.graph[rid]
    ? associations.graph[rid]
    : { metadata: {} };
  const groups = useGroupState(state => state.groups);
  const group = groups[resource?.group] || {};

  const graphs = useGraphState(state => state.graphs);
  const graph = graphs[resourcePath] || null;
  const graphTimesentMap = useGraphState(state => state.graphTimesentMap);

  useEffect(() => {
    api.graph.getGraph(ship, name);
  }, [association]);

  const resourceUrl = `${baseUrl}/resource/link${rid}`;
  if (!graph || !resource) {
    return <Center width='100%' height='100%'><LoadingSpinner /></Center>;
  }
  const { title, description } = resource.metadata;

  const titlebar = (url: string, grid: boolean, back?: string) => (
    <Titlebar back={back} title={title} description={description} workspace={baseUrl} baseUrl={resourceUrl} >
      <Link to={`${url}/${grid ? 'list' : 'grid'}`}>
        <Text bold pr='3' color='blue'>
          Switch to {!grid ? 'grid' : 'list' }
        </Text>
      </Link>

    </Titlebar>
  );

  return (
    <Col alignItems="center" height="100%" width="100%" overflowY="hidden">
      <Switch>
        <Redirect exact from={relativePath('')} to={relativePath('/list')} />
        <Route
          exact
          path={relativePath('/list')}
          render={(props) => {
            return (
              <Col height="100%" width="100%">
                {titlebar(relativePath(''), false)}
                {/* @ts-ignore withState typings*/}
                <LinkWindow
                  key={rid}
                  association={resource}
                  resource={resourcePath}
                  graph={graph}
                  baseUrl={resourceUrl}
                  group={group as Group}
                  path={resource.group}
                  pendingSize={Object.keys(graphTimesentMap[resourcePath] || {}).length}
                  api={api}
                  mb={3}
                />
            </Col>
            );
          }}
        />
        <Route
          exact
          path={relativePath('/grid')}
          render={(props) => {
            return (
              <Col height="100%" width="100%">
                {titlebar(relativePath(''), true)}
                <LinkBlocks graph={graph} association={resource} api={api} />
              </Col>
            );
          }}
        />

        <Route
          path={[relativePath('/list/index/:index'), relativePath('/grid/index/:index')]}
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
            const { pathname } = props.location;
            return (
              <Col height="100%" width="100%">
                {titlebar(relativePath()}
                <LinkDetail
                  node={node}
                  association={association}
                  api={api}
                  baseUrl={pathname}
                />
              </Col>
            );
          }}
        />
      </Switch>
    </Col>
  );
}
