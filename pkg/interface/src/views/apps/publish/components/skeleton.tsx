import React, { useRef, SyntheticEvent, useEffect } from "react";
import { Box, Center } from "@tlon/indigo-react";
import { Sidebar } from "./lib/Sidebar";
import ErrorBoundary from "~/views/components/ErrorBoundary";
import { Notebooks } from "~/types/publish-update";
import { Rolodex } from "~/types/contact-update";
import { Invites } from "~/types/invite-update";
import GlobalApi from "~/logic/api/global";
import { Associations } from "~/types/metadata-update";
import { RouteComponentProps } from "react-router-dom";
import {Graphs} from "~/types/graph-update";

type SkeletonProps = RouteComponentProps<{
  ship?: string;
  notebook?: string;
  noteId?: string;
}> & {
  notebooks: Notebooks;
  invites: Invites;
  associations: Associations;
  contacts: Rolodex;
  graphs: Graphs;
  api: GlobalApi;
  children: React.ReactNode;
};

export function Skeleton(props: SkeletonProps) {
  const { api, notebooks } = props;
  const { ship, notebook, noteId } = props.match.params;
  const scrollRef = useRef<HTMLDivElement>();

  const path =
    (ship &&
      notebook &&
      `/ship/${props.match.params.ship}/${props.match.params.notebook}`) ||
    undefined;

  const onScroll = (e: React.UIEvent<HTMLDivElement>) => {
    const { scrollHeight, scrollTop, clientHeight } = e.target as HTMLDivElement;
    const distanceFromBottom = scrollHeight - scrollTop - clientHeight;
    if (noteId && notebook && ship) {
      const note = notebooks?.[ship]?.[notebook]?.notes?.[noteId];
      if (!note || !note.comments) {
        return;
      }
      const loadedComments = note.comments?.length;
      const fullyLoaded = note["num-comments"] === loadedComments;
      if (distanceFromBottom < 40) {
        if (!fullyLoaded) {
        }
        if (!note.read) {
          // TODO: stubbed for notification-store
        }
      }
    } else if (notebook && ship) {
      // TODO: lazy load notebooks
    }
  };

  // send read immediately if we aren't in a scrollable container
  useEffect(() => {
    if(!(noteId && notebook && ship)) {
      return;
    }
    const note = notebooks?.[ship]?.[notebook]?.notes?.[noteId];
    setTimeout(() => {
      const { clientHeight, scrollHeight } = scrollRef.current;
      const isScrolling = clientHeight < scrollHeight;
      if(!isScrolling && note) {
        // TODO: stubbed for notification-store
      }

    }, 1500);
  }, [noteId, notebook, ship, notebooks])

  const hideSidebar = path || props.location.pathname.endsWith('/new')

  const panelDisplay = !hideSidebar ? ["none", "block"] : "block";
  return (
    <Box height="100%" width="100%" px={[0, 3]} pb={[0, 3]}>
      <Box
        bg="white"
        display="flex"
        border={[0, 1]}
        borderColor={["washedGray", "washedGray"]}
        borderRadius={1}
        width="100%"
        height="100%"
      >
        <Sidebar
          notebooks={props.notebooks}
          graphs={props.graphs}
          contacts={props.contacts}
          path={path}
          hidden={hideSidebar}
          invites={props.invites}
          associations={props.associations}
          api={props.api}
        />
        <Box
          ref={scrollRef}
          display={panelDisplay}
          width="100%"
          height="100%"
          position="relative"
          px={[3, 4]}
          fontSize={0}
          overflowY="scroll"
          onScroll={onScroll}
        >
          <ErrorBoundary>{props.children}</ErrorBoundary>
        </Box>
      </Box>
    </Box>
  );
}

export default Skeleton;
