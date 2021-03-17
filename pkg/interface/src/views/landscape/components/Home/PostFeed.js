import React, {
  useState
} from 'react';
import VirtualScroller from "~/views/components/VirtualScroller";
import PostItem from './Post/PostItem';


export function PostFeed(props) {
  const { graph, graphResource, associations, groups, contacts, api } = props;
  const [isFetching, setIsFetching] = useState(false);

  const renderItem = React.forwardRef(({ index, scrollWindow }, ref) => {
    const node = graph.get(index);
    if (!node) {
      console.log('null');
      return null;
    }

    return (
      <PostItem
        key={index.toString()}
        ref={ref}
        node={node}
        associations={associations}
        groups={groups}
        contacts={contacts}
        api={api}
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
      averageHeight={80}
      size={graph.size}
      style={{ height: '100%' }}
      pendingSize={0}
      renderer={renderItem}
      loadRows={fetchPosts}
    />
  );
}

