import React from 'react';
import bigInt from 'big-integer';
import { Box } from '@tlon/indigo-react'
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

