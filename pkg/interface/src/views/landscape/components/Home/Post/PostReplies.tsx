import { Box, Col, Text } from '@tlon/indigo-react';
import { Association, PermVariation } from '@urbit/api';
import React, { useEffect } from 'react';
import { resourceFromPath } from '~/logic/lib/group';
import useGraphState, { GraphState, useGraph } from '~/logic/state/graph';
import { Loading } from '~/views/components/Loading';
import PostFeed from './PostFeed';
import PostItem from './PostItem/PostItem';
import { stringToArr, arrToString } from '~/views/components/ArrayVirtualScroller';

const graphSel = (s: GraphState) => s.getNode;
import { useGroupForAssoc } from '~/logic/state/group';
import { Switch, useParams, Route, useHistory } from 'react-router';
import bigInt, { BigInteger } from 'big-integer';
import { getNodeFromGraph } from '~/logic/lib/graph';

interface PostRepliesProps {
  baseUrl: string;
  association: Association;
  pendingSize: number;
  vip: PermVariation;
  index?: BigInteger[];
}

export function PostRepliesRoutes(props: PostRepliesProps) {
  const { atom } = useParams<{ atom?: string }>();
  const index = atom
    ? [...(props.index || []), bigInt(atom)]
    : (props.index || []);

  const { baseUrl } = props;
  const makePath = (s: string) => baseUrl + s;

  return (
    <Switch>
      <Route path={makePath('/:atom')} render={(routeProps) => {
        const { url } = routeProps.match;
        return (
          <PostRepliesRoutes {...props} baseUrl={url} index={index} />
          );
      }}
      />
      <Route path={baseUrl}>
        <PostReplies {...props} index={index} />
      </Route>
    </Switch>
  );
}

export default function PostReplies(props: PostRepliesProps) {
  const {
    baseUrl,
    association,
    vip,
    pendingSize,
    index
  } = props;
  const history = useHistory();
  const getNode = useGraphState(graphSel);
  const group = useGroupForAssoc(association);
  const graphPath = association.resource;

  const graphRid = resourceFromPath(graphPath);
  const graph = useGraph(graphRid.ship, graphRid.name);

  const shouldRenderFeed = Boolean(graph);

  useEffect(() => {
    if (graphRid.ship === '~zod' && graphRid.name === 'null') {
      return;
    }

    if (index.length < 1) {
      return;
    }

    getNode(graphRid.ship, graphRid.name, `/${index[0]}`);
  }, [association.resource, index]);

  if (!shouldRenderFeed) {
    return (
      <Box height="100%" width="100%" alignItems="center" pl={1} pt={3}>
        <Loading />
      </Box>
    );
  }

  const node = getNodeFromGraph(graph, index);
  const parentNode = index.length > 1 && getNodeFromGraph(graph, index.slice(0, -1));

  if (!node || !graph) {
    return null;
  }

  const first = node?.children?.peekLargest()?.[0];
  if (!first) {
    return (
      <Col
        width="100%"
        height="100%"
        alignItems="center" overflowY="scroll"
      >
        <Box mt={3} width="100%" alignItems="center">
          <PostItem
            key={node.post.index}
            node={node}
            graphPath={graphPath}
            association={association}
            index={index}
            baseUrl={baseUrl}
            isParent={true}
            parentPost={parentNode?.post}
            vip={vip}
            history={history}
            group={group}
          />
        </Box>
        <Box
          pl={2}
          pr={2}
          width="100%"
          maxWidth="616px"
          alignItems="center"
        >
          <Col bg="washedGray" width="100%" alignItems="center" p={3}>
            <Text textAlign="center" width="100%">
              No one has posted any replies yet.
            </Text>
          </Col>
        </Box>
      </Col>
    );
  }

  return (
    <Box height="calc(100% - 48px)" width="100%" alignItems="center" pl={1} pt={3}>
      <PostFeed
        key={location.pathname}
        graphPath={graphPath}
        graph={node.children}
        grandparentNode={parentNode}
        parentNode={node}
        pendingSize={pendingSize}
        association={association}
        group={group}
        vip={vip}
        baseUrl={baseUrl}
      />
    </Box>
  );
}

