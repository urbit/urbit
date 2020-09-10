import React, { useEffect } from "react";
import { RouteComponentProps, Route, Switch } from "react-router-dom";
import GlobalApi from "~/logic/api/global";
import Notebook from "./Notebook";
import NewPost from "./new-post";
import { NoteRoutes } from './NoteRoutes';
import { Association, Associations, Graphs, Groups, Contacts, Rolodex } from "~/types";

interface NotebookRoutesProps {
  api: GlobalApi;
  ship: string;
  book: string;
  graphs: Graphs;
  notebookContacts: Contacts;
  contacts: Rolodex;
  groups: Groups;
  hideAvatars: boolean;
  hideNicknames: boolean;
  association: Association;
  associations: Associations;
}

export function NotebookRoutes(
  props: NotebookRoutesProps & RouteComponentProps
) {
  const { ship, book, api, notebookContacts } = props;

  useEffect(() => {
    ship && book && api.graph.getGraph(ship, book);
  }, [ship, book]);

  const graph = props.graphs[`${ship.slice(1)}/${book}`];

  const baseUrl = `/~publish/notebook/ship/${ship}/${book}`;

  const relativePath = (path: string) => `${baseUrl}${path}`;
  return (
    <Switch>
      <Route
        path={baseUrl}
        exact
        render={(routeProps) => {
          return <Notebook {...props} graph={graph} />;
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
            association={props.association}
            graph={graph}
          />
        )}
      />
      <Route
        path={relativePath("/note/:noteId")}
        render={(routeProps) => {
          const { noteId } = routeProps.match.params;
          const noteIdNum = parseInt(noteId, 10);

          if(!graph) {
            return null;
          }
          const note = graph.get(noteIdNum);
          if(!note) {
            return null;
          }
          return (
            <NoteRoutes
              api={api}
              book={book}
              ship={ship}
              note={note}
              notebook={graph}
              noteId={noteIdNum}
              contacts={notebookContacts}
              hideAvatars={props.hideAvatars}
              hideNicknames={props.hideNicknames}
              {...routeProps}
            />
          );
        }}
      />
    </Switch>
  );
}
