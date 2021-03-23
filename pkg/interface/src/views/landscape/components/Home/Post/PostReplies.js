import React from 'react';
import bigInt from 'big-integer';
import { Text, Col, Box } from '@tlon/indigo-react'
import { PostInput } from './PostInput';
import { PostFeed } from './PostFeed';
import { Loading } from '~/views/components/Loading';


export function PostReplies(props) {
  const {
    baseUrl,
    api,
    history,
    associations,
    groups,
    contacts,
    graphPath,
    graphs,
    pendingSize,
    graphResource
  } = props;
  const graphId = `${graphResource.ship.slice(1)}/${graphResource.name}`;
  const shouldRenderFeed = graphId in graphs;

  if (!shouldRenderFeed) {
    return (
      <Box height="100%" width="100%" alignItems="center" pl="1" pt="3">
        <Loading />
      </Box>
    );
  }

  const locationUrl =
    history.location.pathname.replace(`${baseUrl}/feed`, '');
  console.log(locationUrl);
  let nodeIndex = locationUrl.split('/').slice(1).map((ind) => {
    return bigInt(ind);
  });

  let node;
  let graph = graphs[graphId];
  nodeIndex.forEach((i) => {
    if (!graph) {
      return null;
    }
    node = graph.get(i);
    if (!node) {
      return null;
    }
    graph = node.children;
  });

  if (!node || !graph) {
    return null;
  }

  const first = graph.peekLargest()?.[0];
  if (!first) {
    return (
      <Col
        key={0}
        width="100%"
        height="100%"
        alignItems="center">
        <Col
          width="100%"
          maxWidth="616px"
          pt="3"
          pl="2"
          pr="2"
          mb="3"
          alignItems="center">
          { shouldRenderFeed ? (
              <PostInput
                api={api}
                index={locationUrl}
                graphResource={graphResource} />
            ) : null
          }
        </Col> 
        <Box
          pl="2"
          pr="2"
          width="100%"
          maxWidth="616px"
          alignItems="center">
          <Col bg="washedGray" width="100%" alignItems="center" p="3">
            <Text textAlign="center" width="100%">
              No one has posted anything here yet.
            </Text>
          </Col>
        </Box>
      </Col>
    );
  }

  return (
    <Box height="100%" width="100%" alignItems="center" pl="1" pt="3">
      <PostFeed
        graphResource={graphResource}
        graph={graph}
        parentNode={node}
        pendingSize={pendingSize}
        associations={associations}
        groups={groups}
        contacts={contacts}
        api={api}
        history={history}
        baseUrl={baseUrl}
      />
    </Box>
  );
}

