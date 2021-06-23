import { Box, Col } from '@tlon/indigo-react';
import { Association, FlatGraph, FlatGraphNode, Group } from '@urbit/api';
import bigInt from 'big-integer';
import React from 'react';
import { RouteComponentProps, useHistory } from 'react-router';
import { resourceFromPath } from '~/logic/lib/group';
import ArrayVirtualScroller, {
  indexEqual,
  arrToString
} from '~/views/components/ArrayVirtualScroller';
import PostItem from './PostItem/PostItem';
import PostInput from './PostInput';
import useGraphState, { GraphState } from '~/logic/state/graph';

const virtualScrollerStyle = {
  height: '100%'
};

interface PostFeedProps {
  flatGraph: FlatGraph;
  graphPath: string;
  getDeepOlderThan: GraphState['getDeepOlderThan'];
  history: RouteComponentProps['history'];
  baseUrl: string;
  parentNode?: FlatGraphNode;
  association: Association;
  group: Group;
  vip: string;
  pendingSize: number;
  isThread: boolean;
}

class PostFlatFeed extends React.Component<PostFeedProps, {}> {
  isFetching: boolean;
  constructor(props) {
    super(props);

    this.isFetching = false;

    this.fetchPosts = this.fetchPosts.bind(this);
    this.doNotFetch = this.doNotFetch.bind(this);
  }

  //  eslint-disable-next-line max-lines-per-function
  renderItem = React.forwardRef<HTMLDivElement, any>(({ index, scrollWindow }, ref) => {
    const {
      flatGraph,
      graphPath,
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

    const key = arrToString(index);

    const first = flatGraph.peekLargest()?.[0];
    const last = flatGraph.peekSmallest()?.[0];
    const isLast = last ? indexEqual(index, last) : false;

    if (indexEqual(index, (first ?? [bigInt.zero]))) {
      if (isThread) {
        return (
          <Col
            pt={3}
            width="100%"
            alignItems="center"
            key={key}
            ref={ref}
          >
            <PostItem
              node={node}
              graphPath={graphPath}
              association={association}
              index={index}
              baseUrl={baseUrl}
              parentPost={parentNode?.post}
              isReply={index.length > 1}
              isRelativeTime={true}
              vip={vip}
              group={group}
              isThread={isThread}
              isLast={isLast}
            />
          </Col>
        );
      }

      return (
        <Col
          width="100%"
          alignItems="center"
          key={key}
          ref={ref}
        >
          <Col
            width="100%"
            maxWidth="608px"
            pt={3}
            pl={1}
            pr={1}
            mb={3}
          >
            <PostInput
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
            index={index}
            baseUrl={baseUrl}
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
          index={index}
          baseUrl={baseUrl}
          parentPost={parentNode?.post}
          isReply={index.length > 1}
          isRelativeTime={true}
          vip={vip}
          group={group}
          isThread={isThread}
          isLast={isLast}
        />
      </Box>
    );
  });

  async fetchPosts(newer) {
    const { flatGraph, graphPath, getDeepOlderThan } = this.props;
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
        await getDeepOlderThan(ship, name, 100, index[0].toString());
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
      history
    } = this.props;

    return (
      <Col width="100%" height="100%" position="relative">
        <ArrayVirtualScroller
          key={history.location.pathname}
          origin="top"
          offset={0}
          data={flatGraph}
          averageHeight={122}
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

export default React.forwardRef<PostFlatFeed, Omit<PostFeedProps, 'history' | 'getDeepOlderThan'>>(
  (props, ref) => {
    const history = useHistory();
    const getDeepOlderThan = useGraphState(s => s.getDeepOlderThan);
    return (
      <PostFlatFeed
        ref={ref}
        {...props}
        history={history}
        getDeepOlderThan={getDeepOlderThan}
      />
    );
  });
