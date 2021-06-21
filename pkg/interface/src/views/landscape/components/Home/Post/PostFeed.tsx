import { Box, Col } from '@tlon/indigo-react';
import { Association, Graph, GraphNode, Group } from '@urbit/api';
import { History } from 'history';
import bigInt from 'big-integer';
import React from 'react';
import { useHistory } from 'react-router';
import { resourceFromPath } from '~/logic/lib/group';
import PostItem from './PostItem/PostItem';
import PostInput from './PostInput';
import { GraphScroller } from '~/views/components/GraphScroller';
import useGraphState, { GraphState } from '~/logic/state/graph';
import shallow from 'zustand/shallow';

const virtualScrollerStyle = {
  height: '100%'
};

interface PostFeedProps extends Pick<GraphState, 'getYoungerSiblings' | 'getOlderSiblings'> {
  graph: Graph;
  graphPath: string;
  history: History;
  baseUrl: string;
  parentNode?: GraphNode;
  grandparentNode?: GraphNode;
  association: Association;
  group: Group;
  vip: string;
  pendingSize: number;
}

class PostFeed extends React.Component<PostFeedProps, any> {
  isFetching: boolean;
  constructor(props) {
    super(props);

    this.isFetching = false;

    this.fetchPosts = this.fetchPosts.bind(this);
    this.doNotFetch = this.doNotFetch.bind(this);
  }
  // @ts-ignore needs @liam-fitzgerald peek at props for virtualscroller
  renderItem = React.forwardRef<HTMLDivElement, any>(({ index, scrollWindow }, ref) => {
    const {
      graph,
      graphPath,
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
              graphPath={graphPath}
              association={association}
              index={nodeIndex}
              baseUrl={baseUrl}
              history={history}
              isParent={true}
              isRelativeTime={false}
              vip={vip}
              group={group}
              isHierarchical={true}
            />
          </Col>
          <PostItem
            node={node}
            graphPath={graphPath}
            association={association}
            index={[...nodeIndex, index]}
            baseUrl={baseUrl}
            history={history}
            isReply={true}
            parentPost={parentNode.post}
            isRelativeTime={true}
            vip={vip}
            group={group}
            isHierarchical={true}
          />
        </React.Fragment>
      );
    } else if (index.eq(first ?? bigInt.zero)) {
      return (
        <Col
          width="100%"
          alignItems="center"
          key={index.toString()}
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
            index={[...nodeIndex, index]}
            baseUrl={baseUrl}
            history={history}
            parentPost={parentNode?.post}
            isReply={Boolean(parentNode)}
            isRelativeTime={true}
            vip={vip}
            group={group}
            isHierarchical={true}
          />
        </Col>
      );
    }

    return (
      <Box key={index.toString()} ref={ref}>
        <PostItem
          node={node}
          graphPath={graphPath}
          association={association}
          index={[...nodeIndex, index]}
          baseUrl={baseUrl}
          history={history}
          parentPost={parentNode?.post}
          isReply={Boolean(parentNode)}
          isRelativeTime={true}
          vip={vip}
          group={group}
          isHierarchical={true}
        />
      </Box>
    );
  });

  async fetchPosts(newer) {
    const { graph, graphPath, getYoungerSiblings, getOlderSiblings } = this.props;
    const graphResource = resourceFromPath(graphPath);

    if (this.isFetching) {
      return false;
    }

    this.isFetching = true;
    const { ship, name } = graphResource;
    const currSize = graph.size;

    if (newer) {
      const [index] = graph.peekLargest();
      await getYoungerSiblings(
        ship,
        name,
        100,
        `/${index.toString()}`
      );
    } else {
      const [index] = graph.peekSmallest();
      await getOlderSiblings(ship, name, 100, `/${index.toString()}`);
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
        <GraphScroller
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

export default React.forwardRef<PostFeed, Omit<PostFeedProps, 'getYoungerSiblings' | 'getOlderSiblings' | 'history'>>((props, ref) => {
  const [getOlderSiblings, getYoungerSiblings] = useGraphState(({ getOlderSiblings, getYoungerSiblings }) => [getOlderSiblings, getYoungerSiblings], shallow);
  const history = useHistory();

  return (
    <PostFeed
      ref={ref}
      {...props}
      history={history}
      getYoungerSiblings={getYoungerSiblings}
      getOlderSiblings={getOlderSiblings}
    />
    );
});

