import React from 'react';
import bigInt from 'big-integer';
import { Text, Col, Box } from '@tlon/indigo-react'
import { PostInput } from './PostInput';
import PostItem from './PostItem/PostItem';
import { PostFeed } from './PostFeed';
import { Loading } from '~/views/components/Loading';
import { resourceFromPath } from '~/logic/lib/group';


export default class PostReplies extends React.PureComponent {
  constructor(props) {
    super(props);
    this.whyDidYouRender = true;
  }

  render() {
    let graph = this.props.graph;
    const {
      baseUrl,
      api,
      history,
      association,
      groups,
      graphPath,
      pendingSize
    } = this.props;

    const graphResource = resourceFromPath(graphPath);
    const graphId = `${graphResource.ship.slice(1)}/${graphResource.name}`;
    const shouldRenderFeed = !!graph;

    if (!shouldRenderFeed) {
      return (
        <Box height="100%" width="100%" alignItems="center" pl="1" pt="3">
          <Loading />
        </Box>
      );
    }

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
          alignItems="center" overflowY="scroll">
          <Box mt="3" width="100%" alignItems="center">
            <PostItem
              key={node.post.index}
              node={node}
              graphPath={graphPath}
              association={association}
              api={api}
              index={nodeIndex}
              baseUrl={baseUrl}
              history={history}
              isParent={true}
            />
          </Box>
          <Box
            pl="2"
            pr="2"
            width="100%"
            maxWidth="616px"
            alignItems="center">
            <Col bg="washedGray" width="100%" alignItems="center" p="3">
              <Text textAlign="center" width="100%">
                No one has posted any replies yet.
              </Text>
            </Col>
          </Box>
        </Col>
      );
    }

    return (
      <Box height="calc(100% - 48px)" width="100%" alignItems="center" pl="1" pt="3">
        <PostFeed
          key={locationUrl}
          graphPath={graphPath}
          graph={graph}
          parentNode={node}
          pendingSize={pendingSize}
          association={association}
          groups={groups}
          api={api}
          history={history}
          baseUrl={baseUrl}
        />
      </Box>
    );
  }
}

