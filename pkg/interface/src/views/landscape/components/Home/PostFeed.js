import React from 'react';
import VirtualScroller from "~/views/components/VirtualScroller";
import { PostItem } from './PostItem';


export function PostFeed(props) {
  const { graph, associations, groups, contacts, api } = props;

  const renderItem = ({ index, scrollWindow }) => {
    const node = graph.get(index);
    if (!node) { return null; }
    return (
      <PostItem
        key={index}
        node={node}
        associations={associations}
        groups={groups}
        contacts={contacts}
        api={api}
      />
    );
  };

  return (
    <VirtualScroller
      origin="top"
      offset={0}
      data={graph}
      averageHeight={100}
      size={graph.size}
      pendingSize={0}
      renderer={renderItem}
      loadRows={async () => true}
    />
  );
}

