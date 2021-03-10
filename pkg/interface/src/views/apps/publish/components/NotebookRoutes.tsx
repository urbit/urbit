import React, { useEffect } from 'react';
import { RouteComponentProps, Route, Switch } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import {
  Association,
  Associations,
  Graphs,
  Groups,
  Contacts,
  Rolodex,
  Unreads,
} from '@urbit/api';
import { Center, LoadingSpinner } from '@tlon/indigo-react';
import { StorageState } from '~/types';
import bigInt from 'big-integer';

import Notebook from './Notebook';
import NewPost from './new-post';
import { NoteRoutes } from './NoteRoutes';

interface NotebookRoutesProps {
  api: GlobalApi;
  ship: string;
  book: string;
  graphs: Graphs;
  unreads: Unreads;
  contacts: Rolodex;
  groups: Groups;
  baseUrl: string;
  rootUrl: string;
  association: Association;
  associations: Associations;
  storage: StorageState;
}

export function NotebookRoutes(
  props: NotebookRoutesProps & RouteComponentProps
) {
  const { ship, book, api, contacts, baseUrl, rootUrl, groups } = props;

  useEffect(() => {
    ship && book && api.graph.getGraph(ship, book);
  }, [ship, book]);

  const graph = props.graphs[`${ship.slice(1)}/${book}`];

  const group = groups?.[props.association?.group];

  const relativePath = (path: string) => `${baseUrl}${path}`;
  return (
    <Switch>
      <Route
        path={baseUrl}
        exact
        render={(routeProps) => {
          if (!graph) {
            return <Center height="100%"><LoadingSpinner /></Center>;
          }
          return <Notebook
            {...props}
            graph={graph}
            contacts={contacts}
            association={props.association}
            rootUrl={rootUrl}
            baseUrl={baseUrl}
                 />;
      }}
      />
      <Route
        path={relativePath('/new')}
        render={routeProps => (
          <NewPost
            {...routeProps}
            api={api}
            book={book}
            ship={ship}
            association={props.association}
            graph={graph}
            baseUrl={baseUrl}
            storage={props.storage}
          />
        )}
      />
      <Route
        path={relativePath('/note/:noteId')}
        render={(routeProps) => {
          const { noteId } = routeProps.match.params;
          const noteIdNum = bigInt(noteId);

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
              unreads={props.unreads}
              noteId={noteIdNum}
              contacts={contacts}
              association={props.association}
              group={group}
              storage={props.storage}
              {...routeProps}
            />
          );
        }}
      />
    </Switch>
  );
}
