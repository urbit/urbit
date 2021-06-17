import { Box, Col, Text } from '@tlon/indigo-react';
import bigInt from 'big-integer';
import React, {
  useEffect
} from 'react';
import { resourceFromPath } from '~/logic/lib/group';
import { Loading } from '~/views/components/Loading';
import { arrToString } from '~/views/components/ArrayVirtualScroller';
import useGraphState from '~/logic/state/graph';
import PostFlatFeed from './PostFlatFeed';
import PostInput from './PostInput';

export default function PostThread(props) {
  const {
    baseUrl,
    association,
    graphPath,
    group,
    vip,
    pendingSize
  } = props;

  const getFirstborn = useGraphState(s => s.getFirstborn);

  const graphResource =
    graphPath ? resourceFromPath(graphPath) : resourceFromPath('/ship/~zod/null');

  const locationUrl = props.locationUrl.replace(`${baseUrl}/feed/thread`, '');
  const index = locationUrl.split('/').slice(1).map(ind => bigInt(ind));

  useEffect(() => {
    if (graphResource.ship === '~zod' && graphResource.name === 'null') {
      return;
    }

    if (index.length < 1) {
      return;
    }

    getFirstborn(
      graphResource.ship,
      graphResource.name,
      arrToString(index)
    );
  }, [graphPath, props.locationUrl]);

  const threadGraphs = useGraphState(state => state.threadGraphs);
  const graphId = `${graphResource.ship.slice(1)}/${graphResource.name}`;

  const shouldRenderFeed =
    graphId in threadGraphs && arrToString(index) in threadGraphs[graphId];

  if (!shouldRenderFeed) {
    return (
      <Box height="100%" pt={3} pb={3} width="100%" alignItems="center" pl={1}>
        <Loading />
      </Box>
    );
  }

  //  TODO: improve performance characteristics of the useGraphState required
  //  to fetch this
  const threadGraph = threadGraphs[graphId][arrToString(index)];

  const first = threadGraph.peekLargest()?.[0];
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

  return (
    <Box height="calc(100% - 48px)" width="100%" alignItems="center" pl={1}>
      <PostFlatFeed
        key={`/thread/${locationUrl}`}
        graphPath={graphPath}
        flatGraph={threadGraph}
        pendingSize={pendingSize}
        association={association}
        group={group}
        vip={vip}
        baseUrl={baseUrl}
        isThread={true}
      />
    </Box>
  );
}

