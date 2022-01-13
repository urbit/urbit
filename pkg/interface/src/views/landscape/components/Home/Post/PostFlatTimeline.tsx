import { Box, Col, Text } from '@tlon/indigo-react';
import { Association, Group } from '@urbit/api';
import React, { ReactElement } from 'react';
import { Loading } from '~/views/components/Loading';
import { useFlatGraph } from '~/logic/state/graph';
import { resourceFromPath } from '~/logic/lib/group';
import PostFlatFeed from './PostFlatFeed';
import PostInput from './PostInput';

interface PostTimelineProps {
  association: Association;
  baseUrl: string;
  graphPath: string;
  group: Group;
  pendingSize: number;
  vip: string;
}

const PostFlatTimeline = (props: PostTimelineProps): ReactElement => {
  const {
    baseUrl,
    association,
    graphPath,
    group,
    pendingSize,
    vip
  } = props;

  const graphRid =
    graphPath ? resourceFromPath(graphPath) : resourceFromPath('/ship/~zod/null');
  const flatGraph = useFlatGraph(graphRid.ship, graphRid.name);

  const shouldRenderFeed = Boolean(flatGraph);

  if (!shouldRenderFeed) {
    return (
      <Box height="100%" pt={3} pb={3} width="100%" alignItems="center" pl={1}>
        <Loading />
      </Box>
    );
  }

  const first = flatGraph.peekLargest()?.[0];
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
          maxWidth="608px"
          pt={3}
          pl={1}
          pr={1}
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
          pl={1}
          pr={1}
          width="100%"
          maxWidth="608px"
          alignItems="center"
        >
          <Col bg="washedGray" width="100%" alignItems="center" p={3}>
            <Text textAlign="center" width="100%">
              No one has posted anything here yet.
            </Text>
          </Col>
        </Box>
      </Col>
    );
  }

  const isGroupLevel = !window.location.href.includes('/feed');

  return (
    <Box height={`calc(100% - ${isGroupLevel ? 165 : 48}px)`} width="100%" alignItems="center" pl={1}>
      <PostFlatFeed
        key={graphPath}
        graphPath={graphPath}
        flatGraph={flatGraph}
        pendingSize={pendingSize}
        association={association}
        group={group}
        vip={vip}
        baseUrl={baseUrl}
        isThread={false}
      />
    </Box>
  );
};

export default PostFlatTimeline;
