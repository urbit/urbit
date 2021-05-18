import { Box, Col } from '@tlon/indigo-react';
import { Association, FlatGraph, FlatGraphNode, Group } from '@urbit/api';
import bigInt from 'big-integer';
import React from 'react';
import { withRouter } from 'react-router';
import GlobalApi from '~/logic/api/global';
import { resourceFromPath } from '~/logic/lib/group';
import ArrayVirtualScroller, {
  indexEqual,
  arrToString,
  stringToArr
} from '~/views/components/ArrayVirtualScroller';
import PostItem from './PostItem/PostItem';
import PostInput from './PostInput';

const virtualScrollerStyle = {
  height: '100%'
};

interface PostFeedProps {
  flatGraph: FlatGraph;
  graphPath: string;
  api: GlobalApi;
  history: History;
  baseUrl: string;
  parentNode?: FlatGraphNode;
  association: Association;
  group: Group;
  vip: string;
  pendingSize: number;
  isThread: boolean;
}

class PostFlatFeed extends React.Component<PostFeedProps, PostFeedState> {
  isFetching: boolean;
  constructor(props) {
    super(props);

    this.isFetching = false;

    this.fetchPosts = this.fetchPosts.bind(this);
    this.doNotFetch = this.doNotFetch.bind(this);
  }

  renderItem = React.forwardRef(({ index, scrollWindow }, ref) => {
    const {
      flatGraph,
      graphPath,
      api,
      history,
      baseUrl,
      association,
      group,
      vip,
      isThread
    } = this.props;

    const node = flatGraph.get(index);
    const parentNode = index.length > 1 ?
      flatGraph.get(index.slice(0, index.length - 1)) : null;

    if (!node) {
      return null;
    }

    let key = arrToString(index);

    const first = flatGraph.peekLargest()?.[0];
    const last = flatGraph.peekSmallest()?.[0];
    const post = node?.post;
    const isLast = last ? indexEqual(index, last) : false;

    if (indexEqual(index, (first ?? [bigInt.zero]))) {
      if (isThread) {
        return (
          <Col
            pt={3}
            width="100%"
            alignItems="center"
            key={key}
            ref={ref}>
            <PostItem
              node={node}
              graphPath={graphPath}
              association={association}
              api={api}
              index={index}
              baseUrl={baseUrl}
              history={history}
              parentPost={parentNode?.post}
              isReply={index.length > 1}
              isRelativeTime={true}
              vip={vip}
              group={group}
              isThread={isThread && !isLast}
            />
          </Col>
        );
      } 

      return (
        <Col
          width="100%"
          alignItems="center"
          key={key}
          ref={ref}>
          <Col
            width="100%"
            maxWidth="608px"
            pt={3}
            pl={1}
            pr={1}
            mb={3}>
            <PostInput
              api={api}
              group={group}
              association={association}
              vip={vip}
              graphPath={graphPath}
            />
          </Col>
          <PostItem
            node={node}
            graphPath={graphPath}
            association={association}
            api={api}
            index={index}
            baseUrl={baseUrl}
            history={history}
            parentPost={parentNode?.post}
            isReply={index.length > 1}
            isRelativeTime={true}
            vip={vip}
            group={group}
          />
        </Col>
      );
    }

    return (
      <Box key={key} ref={ref}>
        <PostItem
          node={node}
          graphPath={graphPath}
          association={association}
          api={api}
          index={index}
          baseUrl={baseUrl}
          history={history}
          parentPost={parentNode?.post}
          isReply={index.length > 1}
          isRelativeTime={true}
          vip={vip}
          group={group}
          isThread={isThread && !isLast}
        />
      </Box>
    );
  });

  async fetchPosts(newer) {
    const { flatGraph, graphPath, api } = this.props;
    const graphResource = resourceFromPath(graphPath);

    if (this.isFetching) {
      return false;
    }

    this.isFetching = true;
    const { ship, name } = graphResource;
    const currSize = flatGraph.size;

    if (newer) {
      return true;
    } else {
      const [index] = flatGraph.peekSmallest();
      if (index && index.length > 0) {
        await api.graph.getDeepOlderThan(ship, name, index[0].toString(), 100);
      } else {
        await api.graph.getDeepOlderThan(ship, name, null, 100);
      }
    }

    this.isFetching = false;
    return currSize === flatGraph.size;
  }

  async doNotFetch(newer) {
    return true;
  }

  render() {
    const {
      flatGraph,
      pendingSize,
      parentNode,
      history
    } = this.props;

    return (
      <Col width="100%" height="100%" position="relative">
        <ArrayVirtualScroller
          key={history.location.pathname}
          origin="top"
          offset={0}
          data={flatGraph}
          averageHeight={106}
          size={flatGraph.size}
          style={virtualScrollerStyle}
          pendingSize={pendingSize}
          renderer={this.renderItem}
          loadRows={this.doNotFetch}
        />
      </Col>
    );
  }
}

export default withRouter(PostFlatFeed);
