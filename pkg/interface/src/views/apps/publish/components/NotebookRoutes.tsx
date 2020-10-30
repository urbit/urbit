import React, { useEffect } from "react";
import { RouteComponentProps, Link, Route, Switch } from "react-router-dom";
import { Center, LoadingSpinner } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { Notebook as INotebook } from "~/types/publish-update";
import { Groups } from "~/types/group-update";
import { Contacts, Rolodex } from "~/types/contact-update";
import { LocalUpdateRemoteContentPolicy, Associations } from "~/types";

import Notebook from "./Notebook";
import NewPost from "./new-post";
import { NoteRoutes } from './NoteRoutes';



interface NotebookRoutesProps {
  api: GlobalApi;
  ship: string;
  book: string;
  notebook: INotebook;
  notebookContacts: Contacts;
  contacts: Rolodex;
  groups: Groups;
  baseUrl?: string;
  rootUrl?: string;
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

  const baseUrl = props.baseUrl || `/~404`;
  const rootUrl = props.rootUrl || '/~404';

  const relativePath = (path: string) => `${baseUrl}${path}`;
  return (
    <Switch>
      <Route
        path={baseUrl}
        exact
        render={(routeProps) => {
          return <Notebook {...props} rootUrl={rootUrl} baseUrl={baseUrl}  />;
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
            baseUrl={baseUrl}
          />
        )}
      />
      <Route
        path={relativePath("/note/:noteId")}
        render={(routeProps) => {
          const { noteId } = routeProps.match.params;
          const note = notebook?.notes?.[noteId];
          const noteUrl = relativePath(`/note/${noteId}`);
          if(!note) {
            return <Center height="100%"><LoadingSpinner /></Center>;
          }
          return (
            <NoteRoutes
              rootUrl={baseUrl}
              baseUrl={noteUrl}
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
