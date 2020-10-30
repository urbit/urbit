import React from "react";
import { Route, Switch } from "react-router-dom";

import GlobalApi from "~/logic/api/global";
import { RouteComponentProps } from "react-router-dom";
import Note from "./Note";
import { EditPost } from "./EditPost";

import { GraphNode, Graph, Contacts, LocalUpdateRemoteContentPolicy } from "~/types";

interface NoteRoutesProps {
  ship: string;
  book: string;
  note: GraphNode;
  noteId: number;
  notebook: Graph;
  contacts: Contacts;
  api: GlobalApi;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  hideNicknames: boolean;
  hideAvatars: boolean;
  baseUrl?: string;
  rootUrl?: string;
}

export function NoteRoutes(props: NoteRoutesProps & RouteComponentProps) {
  const { ship, book, noteId } = props;

  const baseUrl = props.baseUrl || '/~404';
  const rootUrl = props.rootUrl || '/~404';

  const relativePath = (path: string) => `${baseUrl}${path}`;
  return (
    <Switch>
      <Route
        path={relativePath("/edit")}
        render={(routeProps) => <EditPost {...routeProps} {...props} />}
      />
      <Route
        path={baseUrl}
        exact
        render={(routeProps) => {
          return <Note baseUrl={baseUrl} {...routeProps} {...props} rootUrl={rootUrl} />;
        }}
      />

    </Switch>
  );
}
