import React, {
  useState
} from 'react';
import VirtualScroller from "~/views/components/VirtualScroller";
import PostItem from './PostItem';

const virtualScrollerStyle = {
  height: "100%"
};

export function PostFeed(props) {
  const { graph, graphResource, contacts, api, history, baseUrl } = props;
  const [isFetching, setIsFetching] = useState(false);

  const renderItem = React.forwardRef(({ index, scrollWindow }, ref) => {
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

  const fetchPosts = async (newer) => {
    if (isFetching) {
      return false;
    }
    
    setIsFetching(true);
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

    setIsFetching(false);
    return currSize === graph.size;
  };


  return (
    <VirtualScroller
      origin="top"
      offset={0}
      data={graph}
      averageHeight={106}
      size={graph.size}
      style={virtualScrollerStyle}
      pendingSize={0}
      renderer={renderItem}
      loadRows={fetchPosts}
    />
  );
}

