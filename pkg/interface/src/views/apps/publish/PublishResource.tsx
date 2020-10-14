import React, { useEffect, useRef } from "react";
import { Box } from '@tlon/indigo-react';

import GlobalApi from "~/logic/api/global";
import { StoreState } from "~/logic/store/type";
import { Association } from "~/types";
import { RouteComponentProps, useParams, Route, useRouteMatch } from "react-router-dom";
import { NotebookRoutes } from "./components/NotebookRoutes";

type PublishResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
} & RouteComponentProps;

export function PublishResource(props: PublishResourceProps) {
  const { association, api, baseUrl, notebooks } = props;
  const appPath = association["app-path"];
  const [, ship, book] = appPath.split("/");
  const notebook = notebooks[ship]?.[book];
  const notebookContacts = props.contacts[association["group-path"]];
  const match = useRouteMatch<{ noteId: string }>([`${baseUrl}/note/:noteId`, `${baseUrl}/*`, baseUrl]);
  const { noteId } = match?.params || {};
  const scrollRef = useRef();

  const onScroll = (e: React.UIEvent<HTMLDivElement>) => {
    const { scrollHeight, scrollTop, clientHeight } = e.target as HTMLDivElement;
    const distanceFromBottom = scrollHeight - scrollTop - clientHeight;
    if (noteId) {
      const note = notebooks?.[ship]?.[book]?.notes?.[noteId];
      if (!note || !note.comments) {
        return;
      }
      const loadedComments = note.comments?.length;
      const fullyLoaded = note["num-comments"] === loadedComments;
      if (distanceFromBottom < 40) {
        if (!fullyLoaded) {
          api.publish.fetchCommentsPage(
            ship,
            book,
            noteId,
            loadedComments,
            30
          );
        }
        if (!note.read) {
          api.publish.publishAction({
            read: {
              who: ship.slice(1),
              book: notebook,
              note: noteId,
            },
          });
        }
      }
    } 
  };

  // send read immediately if we aren't in a scrollable container
  useEffect(() => {
    if(!noteId) {
      return;
    }
    const note = notebooks?.[ship]?.[book]?.notes?.[noteId];
    setTimeout(() => {
      const { clientHeight, scrollHeight } = scrollRef.current;
      const isScrolling = clientHeight < scrollHeight;
      if(!isScrolling && note) {
        api.publish.publishAction({
          read: {
            who: ship.slice(1),
            book: notebook,
            note: noteId,
          },
        });
      }

    }, 1500);
  }, [noteId])


  return (
    <Box ref={scrollRef} onScroll={onScroll} overflowY="auto">
      <NotebookRoutes
        api={api}
        ship={ship}
        book={book}
        contacts={props.contacts}
        groups={props.groups}
        notebook={notebook}
        associations={props.associations}
        notebookContacts={notebookContacts}
        rootUrl={baseUrl}
        baseUrl={`${baseUrl}/resource/publish/${ship}/${book}`}
        history={props.history}
        match={props.match}
        location={props.location}
        hideAvatars={props.hideAvatars}
        hideNicknames={props.hideNicknames}
        remoteContentPolicy={props.remoteContentPolicy}
      />
    </Box>
  );
}
