import React from "react";
import { Route, Switch } from "react-router-dom";

import { NoteId, Note as INote, Notebook } from "~/types/publish-update";
import { Contacts } from "~/types/contact-update";
import GlobalApi from "~/logic/api/global";
import { RouteComponentProps } from "react-router-dom";
import Note from "./Note";
import { EditPost } from "./EditPost";

interface NoteRoutesProps {
  ship: string;
  book: string;
  noteId: NoteId;
  note: INote;
  notebook: Notebook;
  contacts: Contacts;
  api: GlobalApi;
}

export function NoteRoutes(props: NoteRoutesProps & RouteComponentProps) {
  const { ship, book, noteId } = props;

  const baseUrl = `/~publish/notebook/${ship}/${book}/note/${noteId}`;

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
          return <Note {...routeProps} {...props} />;
        }}
      />

    </Switch>
  );
}
