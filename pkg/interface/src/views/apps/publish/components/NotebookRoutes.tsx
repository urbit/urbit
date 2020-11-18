import React, { useEffect } from "react";
import { RouteComponentProps, Route, Switch } from "react-router-dom";
import GlobalApi from "~/logic/api/global";
import { 
  Association,
  Associations,
  Graphs,
  Groups,
  Contacts,
  Rolodex,
  LocalUpdateRemoteContentPolicy
} from "~/types";
import { Center, LoadingSpinner } from "@tlon/indigo-react";
import { Notebook as INotebook } from "~/types/publish-update";
import bigInt, { BigInteger } from 'big-integer';

import Notebook from "./Notebook";
import NewPost from "./new-post";
import { NoteRoutes } from './NoteRoutes';



interface NotebookRoutesProps {
  api: GlobalApi;
  ship: string;
  book: string;
  graphs: Graphs;
  notebookContacts: Contacts;
  contacts: Rolodex;
  groups: Groups;
  baseUrl: string;
  rootUrl: string;
  hideAvatars: boolean;
  hideNicknames: boolean;
  association: Association;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  associations: Associations;
}

export function NotebookRoutes(
  props: NotebookRoutesProps & RouteComponentProps
) {
  const { ship, book, api, notebookContacts, baseUrl, rootUrl } = props;

  useEffect(() => {
    ship && book && api.graph.getGraph(ship, book);
  }, [ship, book]);

  const graph = props.graphs[`${ship.slice(1)}/${book}`];


  const relativePath = (path: string) => `${baseUrl}${path}`;
  return (
    <Switch>
      <Route
        path={baseUrl}
        exact
        render={(routeProps) => (
          <Notebook
            {...props}
            graph={graph}
            contacts={notebookContacts}
            association={props.association}
            rootUrl={rootUrl}
            baseUrl={baseUrl} />
        )}
      />
      <Route
        path={relativePath("/new")}
        render={(routeProps) => (
          <NewPost
            {...routeProps}
            api={api}
            book={book}
            ship={ship}
            association={props.association}
            graph={graph}
            baseUrl={baseUrl}
          />
        )}
      />
      <Route
        path={relativePath("/note/:noteId")}
        render={(routeProps) => {
          const { noteId } = routeProps.match.params;
          const noteIdNum = bigInt(noteId)

          if(!graph) {
            return <Center height="100%"><LoadingSpinner /></Center>;
          }
          const note = graph.get(noteIdNum);
          if(!note) {
            return <Center height="100%"><LoadingSpinner /></Center>;
          }
          const noteUrl = `${baseUrl}/note/${noteId}`;
          return (
            <NoteRoutes
              rootUrl={baseUrl}
              baseUrl={noteUrl}
              api={api}
              book={book}
              ship={ship}
              note={note}
              notebook={graph}
              noteId={noteIdNum}
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
