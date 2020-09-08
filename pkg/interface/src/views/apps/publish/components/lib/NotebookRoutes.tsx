import React, { useEffect } from "react";
import { RouteComponentProps, Link, Route, Switch } from "react-router-dom";
import { Box, Text } from "@tlon/indigo-react";
import GlobalApi from "../../../../api/global";
import { PublishContent } from "./PublishContent";
import { Notebook as INotebook } from "../../../../types/publish-update";
import { Groups } from "../../../../types/group-update";
import { Contacts, Rolodex } from "../../../../types/contact-update";
import Notebook from "./Notebook";
import NewPost from "./new-post";
import { NoteRoutes } from './NoteRoutes';
import { LocalUpdateRemoteContentPolicy, Associations } from "~/types";

interface NotebookRoutesProps {
  api: GlobalApi;
  ship: string;
  book: string;
  notes: any;
  notebook: INotebook;
  notebookContacts: Contacts;
  contacts: Rolodex;
  groups: Groups;
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  associations: Associations;
}

export function NotebookRoutes(
  props: NotebookRoutesProps & RouteComponentProps
) {
  const { ship, book, api, notebook, notebookContacts } = props;

  useEffect(() => {
    api.publish.fetchNotesPage(ship, book, 1, 50);
    api.publish.fetchNotebook(ship, book);
  }, [ship, book]);


  const baseUrl = `/~publish/notebook/${ship}/${book}`;

  const relativePath = (path: string) => `${baseUrl}${path}`;
  return (
    <Switch>
      <Route
        path={baseUrl}
        exact
        render={(routeProps) => {
          return <Notebook {...props} />;
        }}
      />
      <Route
        path={relativePath("/new")}
        render={(routeProps) => (
          <NewPost
            {...routeProps}
            api={api}
            book={book}
            ship={ship}
            notebook={notebook}
          />
        )}
      />
      <Route
        path={relativePath("/note/:noteId")}
        render={(routeProps) => {
          const { noteId } = routeProps.match.params;
          const note = notebook?.notes[noteId];
          return (
            <NoteRoutes
              api={api}
              book={book}
              ship={ship}
              noteId={noteId}
              notebook={notebook}
              note={note}
              contacts={notebookContacts}
              hideAvatars={props.hideAvatars}
              hideNicknames={props.hideNicknames}
              remoteContentPolicy={props.remoteContentPolicy}
              {...routeProps}
            />
          );
        }}
      />
    </Switch>
  );
}
