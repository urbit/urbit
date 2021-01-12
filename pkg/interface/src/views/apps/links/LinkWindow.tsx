import React, { useRef, useCallback, useEffect } from "react";
import {
  Association,
  Graph,
  Contacts,
  Unreads,
  LocalUpdateRemoteContentPolicy,
  Group,
  Rolodex,
} from "~/types";
import GlobalApi from "~/logic/api/global";
import VirtualScroller from "~/views/components/VirtualScroller";
import { LinkItem } from "./components/LinkItem";

interface LinkWindowProps {
  association: Association;
  contacts: Rolodex;
  resource: string;
  graph: Graph;
  unreads: Unreads;
  hideNicknames: boolean;
  hideAvatars: boolean;
  baseUrl: string;
  group: Group;
  path: string;
  api: GlobalApi;
}
export function LinkWindow(props: LinkWindowProps) {
  const { graph, api, association } = props;
  const loadedNewest = useRef(true);
  const loadedOldest = useRef(false);
  const virtualList = useRef<VirtualScroller>();
  const fetchLinks = useCallback(
    async (newer: boolean) => {
      /* stubbed, should we generalize the display of graphs in virtualscroller? */
    }, []
  );

  useEffect(() => {
    const list = virtualList?.current;
    if(!list) return;
    list.calculateVisibleItems();
  }, [graph.size]);


  return (
    <VirtualScroller
      ref={(l) => (virtualList.current = l ?? undefined)}
      origin="top"
      style={{ height: "100%", width: "100%" }}
      onStartReached={() => {}}
      onScroll={() => {}}
      data={graph}
      size={graph.size}
      renderer={({ index, measure, scrollWindow }) => {
        const node = graph.get(index);
        const post = node?.post;
        if (!node || !post) return null;
        const linkProps = {
          ...props,
          node,
          key: index.toString()
        };
        return <LinkItem {...linkProps} />;
      }}
      loadRows={fetchLinks}
    />
  );
}
