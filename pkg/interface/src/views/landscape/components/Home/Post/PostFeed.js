import React from 'react';
import bigInt from 'big-integer';
import VirtualScroller from "~/views/components/VirtualScroller";
import PostItem from './PostItem';
import { Col } from '@tlon/indigo-react';


const virtualScrollerStyle = {
  height: "100%"
};


export class PostFeed extends React.Component {
  constructor(props) {
    super(props);

    this.isFetching = false;
    this.renderItem = React.forwardRef(({ index, scrollWindow }, ref) => {
      const {
        graph,
        graphResource,
        contacts,
        api,
        history,
        baseUrl,
        parentNode
      } = this.props;
      const node = graph.get(index);
      if (!node) { return null; }

      const first = graph.peekLargest()?.[0];
      const post = node?.post;
      if (!node || !post) {
        return null;
      }
     
      if (parentNode && index.eq(first ?? bigInt.zero)) {
        let nodeIndex = parentNode.post.index.split('/').slice(1).map((ind) => {
          return bigInt(ind);
        });

        return (
          <React.Fragment key={index.toString()}>
            <Col
              key={index.toString()}
              mb="3"
              width="100%"
              flexShrink={0}
            >
              <PostItem
                key={parentNode.post.index}
                ref={ref}
                node={parentNode}
                contacts={contacts}
                graphResource={graphResource}
                api={api}
                index={nodeIndex}
                baseUrl={baseUrl}
                history={history}
                isParent={true}
              />
            </Col>
            <PostItem
              key={index.toString()}
              ref={ref}
              node={node}
              contacts={contacts}
              graphResource={graphResource}
              api={api}
              index={[...nodeIndex, index]}
              baseUrl={baseUrl}
              history={history}
              isReply={true}
              parentPost={parentNode.post}
            />
          </React.Fragment>
        );
      }

      return (
        <PostItem
          key={index.toString()}
          ref={ref}
          node={node}
          contacts={contacts}
          graphResource={graphResource}
          api={api}
          index={[index]}
          baseUrl={baseUrl}
          history={history}
          parentPost={parentNode?.post}
          isReply={!!parentNode}
        />
      );
    });

    this.fetchPosts = this.fetchPosts.bind(this);
    this.doNotFetch = this.doNotFetch.bind(this);
  }

  async fetchPosts(newer) {
    const { graph, graphResource, api } = this.props;

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
    const { graph, pendingSize, parentNode } = this.props;

    return (
      <Col width="100%" height="100%" position="relative">
        <VirtualScroller
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
