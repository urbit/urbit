import { Box, Col } from '@tlon/indigo-react';
import { Association, Graph, GraphNode, Group } from '@urbit/api';
import bigInt from 'big-integer';
import React from 'react';
import GlobalApi from '~/logic/api/global';
import { resourceFromPath } from '~/logic/lib/group';
import VirtualScroller from '~/views/components/VirtualScroller';
import PostItem from './PostItem/PostItem';

const virtualScrollerStyle = {
  height: '100%'
};

interface PostFeedProps {
  graph: Graph;
  graphPath: string;
  api: GlobalApi;
  history?: History;
  baseUrl: string;
  parentNode?: GraphNode;
  grandparentNode?: GraphNode;
  association: Association;
  group: Group;
  vip: string;
  pendingSize: number;
}

export class PostFeed extends React.Component<PostFeedProps, PostFeedState> {
  isFetching: boolean;
  constructor(props) {
    super(props);

    this.isFetching = false;

    this.fetchPosts = this.fetchPosts.bind(this);
    this.doNotFetch = this.doNotFetch.bind(this);
  }

  renderItem = React.forwardRef(({ index, scrollWindow }, ref) => {
    const {
      graph,
      graphPath,
      api,
      history,
      baseUrl,
      parentNode,
      grandparentNode,
      association,
      group,
      vip
    } = this.props;
    const node = graph.get(index);
    if (!node) {
      return null;
    }

    const first = graph.peekLargest()?.[0];
    const post = node?.post;
    const nodeIndex = 
      ( parentNode &&
        typeof parentNode.post !== 'string'
      ) ? parentNode.post.index.split('/').slice(1).map((ind) => {
      return bigInt(ind);
    }) : [];

    if (parentNode && index.eq(first ?? bigInt.zero)) {
      return (
        <React.Fragment key={index.toString()}>
          <Col
            key={index.toString()}
            ref={ref}
            mb={3}
            width="100%"
            flexShrink={0}
          >
            <PostItem
              key={parentNode.post.index}
              parentPost={grandparentNode?.post}
              node={parentNode}
              parentNode={grandparentNode}
              graphPath={graphPath}
              association={association}
              api={api}
              index={nodeIndex}
              baseUrl={baseUrl}
              history={history}
              isParent={true}
              isRelativeTime={false}
              vip={vip}
              group={group}
            />
          </Col>
          <PostItem
            node={node}
            graphPath={graphPath}
            association={association}
            api={api}
            index={[...nodeIndex, index]}
            baseUrl={baseUrl}
            history={history}
            isReply={true}
            parentPost={parentNode.post}
            isRelativeTime={true}
            vip={vip}
            group={group}
          />
        </React.Fragment>
      );
    }

    return (
      <Box key={index.toString()} ref={ref}>
      <PostItem
        node={node}
        graphPath={graphPath}
        association={association}
        api={api}
        index={[...nodeIndex, index]}
        baseUrl={baseUrl}
        history={history}
        parentPost={parentNode?.post}
        isReply={Boolean(parentNode)}
        isRelativeTime={true}
        vip={vip}
        group={group}
      />
      </Box>
    );
  });

  async fetchPosts(newer) {
    const { graph, graphPath, api } = this.props;
    const graphResource = resourceFromPath(graphPath);

    if (this.isFetching) {
      return false;
    }

    this.isFetching = true;
    const { ship, name } = graphResource;
    const currSize = graph.size;

    if (newer) {
      const [index] = graph.peekLargest();
      await api.graph.getYoungerSiblings(
        ship,
        name,
        100,
        `/${index.toString()}`
      );
    } else {
      const [index] = graph.peekSmallest();
      await api.graph.getOlderSiblings(ship, name, 100, `/${index.toString()}`);
    }

    this.isFetching = false;
    return currSize === graph.size;
  }

  async doNotFetch(newer) {
    return true;
  }

  render() {
    const {
      graph,
      pendingSize,
      parentNode,
      history
    } = this.props;

    return (
      <Col width="100%" height="100%" position="relative">
        <VirtualScroller
          key={history.location.pathname}
          origin="top"
          offset={0}
          data={graph}
          averageHeight={106}
          size={graph.size}
          style={virtualScrollerStyle}
          pendingSize={pendingSize}
          renderer={this.renderItem}
          loadRows={parentNode ? this.doNotFetch : this.fetchPosts}
        />
      </Col>
    );
  }
}
