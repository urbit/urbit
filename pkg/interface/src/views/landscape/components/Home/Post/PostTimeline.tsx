import { Box, Col, Text } from '@tlon/indigo-react';
import { Association, Graph, Group } from '@urbit/api';
import { History } from 'history';
import React, { ReactElement } from 'react';
import { Loading } from '~/views/components/Loading';
import PostFeed from './PostFeed';
import PostInput from './PostInput';

interface PostTimelineProps {
  association: Association;
  baseUrl: string;
  graph: Graph;
  graphPath: string;
  group: Group;
  pendingSize: number;
  vip: string;
  history?: History;
}

const PostTimeline = (props: PostTimelineProps): ReactElement => {
  const {
    baseUrl,
    association,
    graphPath,
    group,
    graph,
    pendingSize,
    vip
  } = props;
  const shouldRenderFeed = Boolean(graph);


  if (!shouldRenderFeed) {
    return (
      <Box height="100%" pt={3} pb={3} width="100%" alignItems="center" pl={1}>
        <Loading />
      </Box>
    );
  }

  const first = graph.peekLargest()?.[0];
  if (!first) {
    return (
      <Col
        key={0}
        width="100%"
        height="100%"
        alignItems="center"
      >
        <Col
          width="100%"
          maxWidth="616px"
          pt={3}
          pl={2}
          pr={2}
          mb={3}
          alignItems="center"
        >
          <PostInput
            graphPath={graphPath}
            group={group}
            association={association}
            vip={vip}
          />
        </Col>
        <Box
          pl={2}
          pr={2}
          width="100%"
          maxWidth="616px"
          alignItems="center"
        >
          <Col bg="washedGray" width="100%" alignItems="center" p={3}>
            <Text textAlign="center" width="100%">
              No one has posted anything here yet!.
            </Text>
          </Col>
        </Box>
      </Col>
    );
  }

  return (
    <Box height="calc(100% - 48px)" width="100%" alignItems="center" pl={1}>
      <PostFeed
        key={graphPath}
        graphPath={graphPath}
        graph={graph}
        pendingSize={pendingSize}
        association={association}
        group={group}
        vip={vip}
        baseUrl={baseUrl}
      />
    </Box>
  );
};

export default PostTimeline;
