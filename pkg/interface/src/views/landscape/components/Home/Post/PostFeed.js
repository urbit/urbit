import React from 'react';
import VirtualScroller from "~/views/components/VirtualScroller";
import PostItem from './PostItem';

const virtualScrollerStyle = {
  height: "100%"
};


export class PostFeed extends React.Component {
  constructor(props) {
    super(props);

    this.isFetching = false;
    this.renderItem = React.forwardRef(({ index, scrollWindow }, ref) => {
      const { graph, graphResource, contacts, api, history, baseUrl } = this.props;
      const node = graph.get(index);
      if (!node) { return null; }

      return (
        <PostItem
          key={index.toString()}
          ref={ref}
          node={node}
          contacts={contacts}
          graphResource={graphResource}
          api={api}
          index={index}
          baseUrl={baseUrl}
          history={history}
        />
      );
    });

    this.fetchPosts = this.fetchPosts.bind(this);
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

  render() {
    const { graph, pendingSize } = this.props;

    return (
      <VirtualScroller
        origin="top"
        offset={0}
        data={graph}
        averageHeight={106}
        size={graph.size}
        style={virtualScrollerStyle}
        pendingSize={pendingSize}
        renderer={this.renderItem}
        loadRows={this.fetchPosts}
      />
    );
  }
}
