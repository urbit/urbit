import { Box, Center, Col, LoadingSpinner, Text } from '@tlon/indigo-react';
import { Group } from '@urbit/api';
import { Association } from '@urbit/api/metadata';
import bigInt from 'big-integer';
import React, { useEffect } from 'react';
import { Link, Route, Switch } from 'react-router-dom';
import useGraphState from '~/logic/state/graph';
import useMetadataState from '~/logic/state/metadata';
import { StoreState } from '~/logic/store/type';
import { Comments } from '~/views/components/Comments';
import useGroupState from '../../../logic/state/group';
import { LinkItem } from './components/LinkItem';
import './css/custom.css';
import LinkWindow from './LinkWindow';

const emptyMeasure = () => {};

type LinkResourceProps = StoreState & {
  association: Association;
  baseUrl: string;
};

export function LinkResource(props: LinkResourceProps) {
  const {
    association,
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
  const getGraph = useGraphState(s => s.getGraph);

  useEffect(() => {
    getGraph(ship, name);
  }, [association]);

  const resourceUrl = `${baseUrl}/resource/link${rid}`;
  if (!graph) {
    return <Center width='100%' height='100%'><LoadingSpinner /></Center>;
  }

  return (
    <Col alignItems="center" height="100%" width="100%" overflowY="hidden">
      <Switch>
        <Route
          exact
          path={relativePath('')}
          render={(props) => {
            return (
              // @ts-ignore state helper weirdness
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
            );
          }}
        />
        <Route
          path={relativePath('/index/:index')}
          render={(props) => {
            const index = bigInt(props.match.params.index);
            const editCommentId = props.match.params.commentId || null;

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
              <Col alignItems="center" overflowY="auto" width="100%">
              <Col width="100%" p={3} maxWidth="768px">
                <Link to={resourceUrl}><Text px={3} bold>{'<- Back'}</Text></Link>
                <LinkItem
                  key={node.post.index}
                  resource={resourcePath}
                  node={node}
                  baseUrl={resourceUrl}
                  association={association}
                  group={group as Group}
                  path={resource?.group}
                  mt={3}
                  measure={emptyMeasure}
                />
                <Comments
                  ship={ship}
                  name={name}
                  comments={node}
                  resource={resourcePath}
                  association={association}
                  editCommentId={editCommentId}
                  history={props.history}
                  baseUrl={`${resourceUrl}/index/${props.match.params.index}`}
                  group={group as Group}
                  px={3}
                />
              </Col>
            </Col>
            );
          }}
        />
      </Switch>
    </Col>
  );
}
