import React from 'react';
import { Box, Row, Text } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';
import bigInt from 'big-integer';


export function GroupFeedHeader(props) {
  const { baseUrl, history, graphs, graphResource } = props;
  const historyLocation = history.location.pathname;
  const graphId = `${graphResource.ship.slice(1)}/${graphResource.name}`;
  const shouldRenderFeed = graphId in graphs;

  const isHome =
    historyLocation === baseUrl ||
    historyLocation === `${baseUrl}/feed`;

  const locationUrl =
    history.location.pathname.replace(`${baseUrl}`, '').replace(/^\/[a-z]*/, '');
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

  let authorText = '';
  if (node) {
    authorText = node.post.author;
  }

  return (
    <Row 
      flexShrink="0"
      width="100%"
      height="48px"
      pl="2"
      pr="2"
      alignItems="center"
      borderBottom={1}
      borderColor="washedGray">
      <Box display='block'>
        { ( baseUrl !== historyLocation &&
            `${baseUrl}/feed` !== historyLocation
          ) ? (
            <Text pr="2" cursor="pointer" onClick={() => {
              history.goBack();
            }}>{'<- Back'}</Text>
          ) : null
        }
      </Box>
      { isHome ? (
        <>
          <Text bold fontSize="2" pr="2">Group Feed</Text>
          <Text fontSize="0" p="1" backgroundColor="washedGray">Everyone can post</Text>
        </>
      ) : ( !authorText ? null : (
        <>
          <Text bold fontSize="2" pr="2">Post by </Text>
          <Text bold fontSize="2" mono>{`~${authorText}`}</Text>
        </>
      ))}
    </Row>
  );
}


