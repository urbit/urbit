import React from 'react';
import { Text, Col, Box } from '@tlon/indigo-react'
import { PostInput } from './PostInput';
import { PostFeed } from './PostFeed';
import { Loading } from '~/views/components/Loading';
import { resourceFromPath } from '~/logic/lib/group';


export default class PostTimeline extends React.PureComponent {
  constructor(props) {
    super(props);
    this.whyDidYouRender = true;
  }

  render() {
    const {
      baseUrl,
      api,
      history,
      association,
      graphPath,
      graph,
      pendingSize,
    } = this.props;
    const graphResource = resourceFromPath(graphPath);
    const shouldRenderFeed = !!graph;

    if (!shouldRenderFeed) {
      return (
        <Box height="100%" pt="3" pb="3" width="100%" alignItems="center" pl="1">
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
          alignItems="center">
          <Col
            width="100%"
            maxWidth="616px"
            pt="3"
            pl="2"
            pr="2"
            mb="3"
            alignItems="center">
            <PostInput
              api={api}
              graphPath={graphPath} />
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
      <>
        <Box
          width="100%"
          maxWidth="616px"
          pt="3"
          pl="2"
          pr="2"
          mb="3"
          flexDirection="column"
          alignItems="center">
          <PostInput api={api} graphPath={graphPath} />
        </Box> 
        <Box height="calc(100% - 176px)" width="100%" alignItems="center" pl="1">
          <PostFeed
            key={graphPath}
            graphPath={graphPath}
            graph={graph}
            pendingSize={pendingSize}
            association={association}
            api={api}
            history={history}
            baseUrl={baseUrl}
          />
        </Box>
      </>
    );
  }
}

