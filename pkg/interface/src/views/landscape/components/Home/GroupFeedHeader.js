import React from 'react';
import { Box, Row, Text } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';
import bigInt from 'big-integer';


export function GroupFeedHeader(props) {
  const { baseUrl, history, graphResource, vip } = props;
  let graph = props.graph;
  const historyLocation = history.location.pathname;
  const graphId = `${graphResource.ship.slice(1)}/${graphResource.name}`;

  const isHome =
    historyLocation === baseUrl ||
    historyLocation === `${baseUrl}/feed`;

  const locationUrl =
    history.location.pathname.replace(`${baseUrl}/feed`, '');
  let nodeIndex = locationUrl.split('/').slice(1).map((ind) => {
    return bigInt(ind);
  });

  let node;
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

  const permText = (vip === 'host-feed') 
    ?  'Only host can post'
    : vip === 'admin-feed'
    ? 'Only admins can post'
    : 'Everyone can post';

  return (
    <Row 
      flexShrink="0"
      width="100%"
      height="48px"
      pl="2"
      pr="2"
      alignItems="center"
      borderBottom={1}
      borderColor="lightGray">
      <Box display='block'>
        { ( baseUrl !== historyLocation ) ? (
            <Text pl="1" pr="1" cursor="pointer" onClick={() => {
              let loc = locationUrl.split('/');
              loc.pop();
              loc = loc.join('/');
              history.push(`${baseUrl}${loc}`);
            }}>{'<- Back'}</Text>
          ) : null
        }
      </Box>
      { isHome ? (
        <>
          <Text bold fontSize="2" pl="1" pr="2">Group Feed</Text>
          <Text fontSize="0" p="1" backgroundColor="washedGray">{permText}</Text>
        </>
      ) : ( !authorText ? null : (
        <>
          <Text bold fontSize="2" pl="1" pr="2">Post by </Text>
          <Text bold fontSize="2" mono>{`~${authorText}`}</Text>
        </>
      ))}
    </Row>
  );
}


